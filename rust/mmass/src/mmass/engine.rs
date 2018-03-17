use std::thread;
use std::time;
use std::sync;

use mmass::config as config;
use mmass::local_env as local_env;

#[derive(Debug)]
enum EngineState {
  Main,
  Running,
  Paused,
  Final
}

/* Messages to the engine prefixed with 'Msg' do not expect a response.
 * Messagew prefixed with 'Req' are expected to be responded by the corresponding 'Res' message.
 */
#[derive(Debug)]
pub enum EngineMessage {
  ReqGenerate{ name: String },
  ResGenerate{ env: Box<local_env::LocalEnv> },
  MsgStart{ world_name: String, scenario_name: String },
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
  scenario: Option<config::Scenario>,
  local_env: Option<local_env::LocalEnv>,
}

impl Engine {
  pub fn new(
      engine_mailbox: sync::mpsc::Receiver<EngineMessage>,
      repl_mailbox: sync::mpsc::Sender<EngineMessage>,
      config: config::Config,
    ) -> thread::JoinHandle<u32> {
    let mut engine = Engine{
      engine_mailbox: engine_mailbox,
      repl_mailbox: repl_mailbox,
      config: config,
      state: EngineState::Main,
      scenario: None,
      local_env: None,
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
      EngineMessage::MsgStart{ world_name, scenario_name } => {
        debug!("Message: MsgStart");
        debug!("Starting simulation in world {} under {} scenario", world_name, scenario_name);
        // TODO deserialize the world
        // TODO fetch and set the scenario parameters
        // TODO initialize the simulation
        self.state = EngineState::Paused;
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
        unimplemented!();
      }
      EngineMessage::MsgStep{ steps: _steps } => {
        debug!("Message: MsgStep.");
        unimplemented!();
      }
      EngineMessage::MsgSave{ savefile: _savefile } => {
        debug!("Message: MsgSave.");
        unimplemented!();
      }
      EngineMessage::MsgStop => {
        debug!("Message: MsgStop.");
        // TODO Save and go to main state.
        self.state = EngineState::Main;
        unimplemented!();
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
        unimplemented!();
      }
      EngineMessage::MsgStop => {
        debug!("Message: MsgStop.");
        self.state = EngineState::Main;
        unimplemented!();
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
}

