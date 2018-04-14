use std::io::prelude::*;
use std::env;
use std::fs;
use std::path;
use std::thread;
use std::time;
use std::sync;

use bincode;

use mmass::config as config;
use mmass::local_env as local_env;
use mmass::agent as agent;
use mmass::action as action;

/* Component Manager
 */
#[derive(Debug, Deserialize, Serialize)]
struct Components {
  // TODO user input component, tie into joystick agent
  // TODO move dynamic data components such as actions into the component manager
  // 
}

// impl Components {
//   pub fn new() -> Components {
//     return Components
//   }

//   pub fn add() {}
//   pub fn remove() {}
//   pub fn clear() {}
// }

#[derive(Debug, Deserialize, Serialize)]
struct Simulation {
  pub scenario: Option<config::Scenario>,
  pub env: Option<local_env::LocalEnv>,
  pub fi: u64, // Frame index
  pub ti: u64, // Time index
  pub components: Components,
}

#[derive(Debug)]
enum EngineState {
  Main,
  Running,
  Paused,
  Final
}

#[derive(Debug)]
pub enum EngineMessageResult {
  Ok,
  Err,
}

/* Messages to the engine prefixed with 'Msg' do not expect a response.
 * Messagew prefixed with 'Req' expect a response message of the corresponding 'Res' type.
 */
#[derive(Debug)]
pub enum EngineMessage {
  ReqGenerate{ name: String },
  ResGenerate{ env: Box<local_env::LocalEnv> },
  ReqStart{ scenario_name: String, world_name: String },
  ResStart{ error: EngineMessageResult },
  MsgLoad{ savefile: String },
  MsgRun,
  MsgPause,
  MsgStep{ steps: u8 },
  MsgSave{ savefile: String },
  MsgStop,
  MsgQuit,
}

pub struct Engine {
  engine_mailbox: sync::mpsc::Receiver<EngineMessage>,
  repl_mailbox: sync::mpsc::Sender<EngineMessage>,
  config: config::Config,
  state: EngineState,
  sim: Simulation,
}

impl Engine {
  pub fn new(
      engine_mailbox: sync::mpsc::Receiver<EngineMessage>,
      repl_mailbox: sync::mpsc::Sender<EngineMessage>,
      config: config::Config,
    ) -> thread::JoinHandle<u32> {
    let sim = Simulation{
      scenario: None,
      env: None,
      fi: 0,
      ti: 0,
      components: Components{},
    };
    let mut engine = Engine{
      engine_mailbox: engine_mailbox,
      repl_mailbox: repl_mailbox,
      config: config,
      state: EngineState::Main,
      sim: sim,
      };
    let builder = thread::Builder::new().name("Engine".into());
    let handle = builder.spawn(move || { engine.run(); 0 }).unwrap();
    return handle
  }

  pub fn run(&mut self) {
    loop {
      match self.state {
        EngineState::Main => self.state_main(),
        EngineState::Running => self.state_running(),
        EngineState::Paused => self.state_paused(),
        EngineState::Final => {
          self.state_final();
          break;
        }
      }
    }
  }

  fn state_main(&mut self) {
    debug!("State: Main");
    match self.engine_mailbox.recv() {
      Ok(msg) => self.handle_msg_main(msg),
      Err(sync::mpsc::RecvError) => {
        error!("Receive error.");
        panic!();
      }
    }
  }

  fn state_paused(&mut self) {
    debug!("State: Paused");
    match self.engine_mailbox.recv() {
      Ok(msg) => self.handle_msg_paused(msg),
      Err(sync::mpsc::RecvError) => {
        error!("Receive error.");
        panic!();
      }
    }
  }

  fn state_running(&mut self) {
    debug!("State: Running");
    match self.engine_mailbox.try_recv() {
      Ok(msg) => self.handle_msg_running(msg),
      Err(sync::mpsc::TryRecvError::Empty) => {
        self.sim_frame();
        thread::sleep(time::Duration::from_millis(500));
      }
      Err(sync::mpsc::TryRecvError::Disconnected) => {
        error!("Receiver disconnected.");
        panic!();
      }
    }
  }

  fn state_final(&mut self) {
    debug!("State: Final");
  }

  fn handle_msg_main(&mut self, msg: EngineMessage) {
    match msg {
      EngineMessage::ReqGenerate{ name } => {
        debug!("Message: MsgGenerate");
        let env = local_env::LocalEnv::generate(name.clone(), 10, 10);
        let msg = EngineMessage::ResGenerate{ env: Box::new(env) };
        self.repl_mailbox.send(msg).unwrap();
      }
      EngineMessage::ReqStart{ scenario_name, world_name } => {
        debug!("Message: MsgStart");
        debug!("Starting simulation in world {} under {} scenario", world_name, scenario_name);

        // Initialize the simulation
        match self.init_sim(scenario_name, world_name) {
          Ok(_) => {
            let msg = EngineMessage::ResStart{ error: EngineMessageResult::Ok };
            self.repl_mailbox.send(msg).unwrap();
            self.state = EngineState::Paused;
          }
          Err(_) => {
            // TODO Better specify the error.
            let msg = EngineMessage::ResStart{ error: EngineMessageResult::Err };
            self.repl_mailbox.send(msg).unwrap();
          }
        }
      }
      EngineMessage::MsgLoad{ savefile: _savefile } => {
        debug!("Message: MsgLoad");
        self.state = EngineState::Paused;
        unimplemented!();
      }
      EngineMessage::MsgQuit => {
        debug!("Message: MsgQuit");
        self.state = EngineState::Final;
      }
      msg => {
        error!("Unexpected message. state: {:?} msg: {:?}", self.state, msg);
      }
    }
  }

  fn handle_msg_paused(&mut self, msg: EngineMessage) {
    match msg {
      EngineMessage::MsgRun => {
        debug!("Message: MsgRun.");
        self.state = EngineState::Running;
      }
      EngineMessage::MsgStep{ steps } => {
        debug!("Message: MsgStep.");
        for _ in 0..steps {
          self.sim_frame();
        }
      }
      EngineMessage::MsgSave{ savefile: _savefile } => {
        debug!("Message: MsgSave.");
        self.save_sim();
      }
      EngineMessage::MsgStop => {
        debug!("Message: MsgStop.");
        self.save_sim();
        self.state = EngineState::Main;
      }
      EngineMessage::MsgQuit => {
        debug!("Message: MsgQuit.");
        self.state = EngineState::Final;
      }
      msg => {
        error!("Unexpected message. state: {:?} msg: {:?}", self.state, msg);
      }
    }
  }

  fn handle_msg_running(&mut self, msg: EngineMessage) {
    match msg {
      EngineMessage::MsgPause => {
        debug!("Message: MsgPause.");
        self.state = EngineState::Paused;
      }
      EngineMessage::MsgStop => {
        debug!("Message: MsgStop.");
        self.state = EngineState::Main;
      }
      EngineMessage::MsgQuit => {
        debug!("Message: MsgQuit.");
        self.state = EngineState::Final;
      }
      msg => {
        error!("Unexpected message. state: {:?} msg: {:?}", self.state, msg);
      }
    }
  }

  // TODO Improve function return type to provide better information.
  fn init_sim(&mut self, scenario_name: String, world_name: String) -> Result<(), ()> {
    println!("Initializing simulation...");
    // Deserialize the world
    let world_path = world_path(&world_name);

    match fs::File::open(&world_path) {
      Ok(mut file) => {
        let mut encoded: Vec<u8> = Vec::new();
        file.read_to_end(&mut encoded).unwrap();
        let mut env: local_env::LocalEnv = bincode::deserialize(&encoded).unwrap();

        // Fetch and set the scenario parameters
        for scenario in self.config.scenarios.iter() {
          if scenario_name == scenario.name {
            self.sim.scenario = Some(scenario.clone());
            // Instantiate the agents.
            match scenario.starting_units {
              Some(ref units) => {
                for agent_description in units.iter() {
                  // TODO Give agents a random position.
                  let position = local_env::Position{ x: 0, y: 0 };
                  let agent = agent::Agent::new(&agent_description.kind, position);
                  env.agents.add(agent);
                }
              }
              None => {}
            }
          }
        }
        self.sim.env = Some(env);
        return Ok(())
      }
      Err(why) => {
        error!("Couldn't open {}: {}", world_path.display(), why);
        return Err(());
      }
    }
  }

  fn sim_frame(&mut self) {
    self.sim.fi += 1;
    println!("Running simulation frame {}", self.sim.fi);
    physics(&mut self.sim);
    events(&mut self.sim);
    agents(&mut self.sim);
    actions(&mut self.sim);
  }

  fn save_sim(&mut self) {
    // TODO
    println!("Saving simulation state...");
  }
}

// Systems
// TODO Move systems out into their own modules

fn physics(_sim: &mut Simulation) {
  // TODO
}

fn events(_sim: &mut Simulation) {
  // TODO
}

fn agents(sim: &mut Simulation) {
  match sim.env {
    Some(ref mut env) => {
      // TODO hide the data component of agent and action containers
      for (id, agent) in env.agents.data.iter() {
        let action = agent.action();
        env.actions.add(*id, action);
      }
    }
    None => {
      error!("No environment!");
      panic!();
    }
  }
}

fn actions(sim: &mut Simulation) {
  /*
  for each action
    if the action conflicts with other actions
      resolve conflict and mark it as not done for the agent's next percept
    else
      if the action's precondition is true
        apply the action's postcondition
  */
  match sim.env {
    Some(ref mut env) => {
      for (agent_id, action) in env.actions.data.iter() {
        match action.data {
          action::ActionData::Noop => {
            println!("Agent {} does nothing.", agent_id);
          },
          action::ActionData::Move{ ref direction } => {
            println!("Agent {} moves {:?}.", agent_id, direction);
          },
          action::ActionData::GoTo{ ref position } => {
            println!("Agent {} goes to {:?}", agent_id, position);
          }
        }
      }
      env.actions.clear();
    }
    None => {
      error!("No environment!");
      panic!();
    }
  }
}

// Utilities
fn world_path(name: &String) -> path::PathBuf {
  let mut path = env::current_dir().unwrap();
  path.push("worlds");
  path.push(&name);
  path.set_extension("bin");
  return path
}