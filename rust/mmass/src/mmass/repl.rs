use std::io;
use std::io::Write;
use std::io::BufRead;
use std::collections::HashMap;
use std::thread;
use std::sync;
use serde_yaml;

use mmass::config as config;
use mmass::scenario as scenario;
use mmass::engine as engine;
use mmass::local_env as local_env;

pub enum ReplState {
  Init,
  Running,
  Generating,
  Final,
}

pub enum ReplCommand {
  Help,
  List{ target: String },
  WorldGen { name: String },
  Accept,
  Reject,
  Start { world_name: String, scenario_name: String },
  Load { savefile: String },
  Run,
  Order { params: HashMap<String, String> },
  Pause,
  Step { steps: u8 },
  Save { savefile: String },
  Stop,
  Inspect { target: String },
  Quit,
}

pub struct Repl {
  repl_mailbox: sync::mpsc::Receiver<engine::EngineMessage>,
  engine_mailbox: sync::mpsc::Sender<engine::EngineMessage>,
  state: ReplState,
  config: config::Config,
  scenario_config: scenario::ScenarioConfig,
  env: Option<local_env::LocalEnv>,
}

impl Repl {
  pub fn new(
      repl_mailbox: sync::mpsc::Receiver<engine::EngineMessage>,
      engine_mailbox: sync::mpsc::Sender<engine::EngineMessage>,
      config: config::Config,
      scenario_config: scenario::ScenarioConfig,
      ) -> thread::JoinHandle<u32>
  {
    let mut repl = Repl{
      repl_mailbox: repl_mailbox,
      engine_mailbox: engine_mailbox,
      state: ReplState::Init,
      config: config,
      scenario_config: scenario_config,
      env: None,
      };
    let builder = thread::Builder::new().name("REPL".into());
    let handle = builder.spawn(move || { repl.run(); 0 }).unwrap();
    return handle
  }

  fn run(&mut self) {
    loop {
      let input = Repl::prompt();
      match Repl::parse(input) {
        Ok(ReplCommand::Quit) => {
          self.state_final(&ReplCommand::Quit);
          break;
        }
        Ok(command) => {
          match self.state {
            ReplState::Init => self.state_init(&command),
            ReplState::Running => self.state_running(&command),
            ReplState::Generating => self.state_generating(&command),
            ReplState::Final => {}
          }
        }
        Err(reason) => println!("Error: {}", reason),
      }
    }
  }

  fn prompt() -> String {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    print!("> ");
    stdout.flush().expect("Error flushing stdout.");
    let mut input = String::new();
    stdin.lock().read_line(&mut input).expect("Error reading from stdin.");
    return input
  }

  fn parse(input: String) -> Result<ReplCommand, String> {
    let tokens: Vec<&str> = input.trim().split(' ').collect();
    match tokens[0] {
      "help" => return Ok(ReplCommand::Help),
      "list" => {
        if 1 < tokens.len() {
          let target = tokens[1].to_string();
          return Ok(ReplCommand::List{ target })
        } else {
          return Err("Input error: list takes a parameter.".to_string())
        }
      }
      "worldgen" => {
        if 1 < tokens.len() {
          let name = tokens[1].to_string();
          return Ok(ReplCommand::WorldGen{ name })
        } else {
          return Err("Input error: inspect takes a parameter.".to_string())
        }
      }
      "accept" => return Ok(ReplCommand::Accept),
      "reject" => return Ok(ReplCommand::Reject),
      "start" => {
        if 2 < tokens.len() {
          let world_name = tokens[1].to_string();
          let scenario_name = tokens[2].to_string();
          return Ok(ReplCommand::Start{ world_name: world_name, scenario_name: scenario_name })
        } else {
          return Err("Input error: start takes two parameters.".to_string())
        }
      } 
      "load" => {
        let savefile = tokens[1].to_string();
        return Ok(ReplCommand::Load{ savefile })
      }
      "run" => return Ok(ReplCommand::Run),
      "order" => {
        // TODO
        let params = HashMap::new();
        return Ok(ReplCommand::Order{ params })
      }
      "pause" => return Ok(ReplCommand::Pause),
      "step" => {
        if 1 < tokens.len() {
          match tokens[1].parse::<u8>() {
            Ok(steps) => return Ok(ReplCommand::Step{ steps } ),
            Err(_reason) => return Err("Cannot convert the given value to an integer.".to_string()),
          }
        } else {
          return Ok(ReplCommand::Step{ steps: 1 } )
        }
      }
      "save" => {
        let savefile = tokens[1].to_string();
        return Ok(ReplCommand::Save{ savefile })
      }
      "stop" => return Ok(ReplCommand::Stop),
      "inspect" => {
        if 1 < tokens.len() {
          let target  = tokens[1].to_string();
          return Ok(ReplCommand::Inspect{ target: target })
        } else {
          return Err("Input error: inspect needs a parameter.".to_string())
        }
      }
      "quit" => return Ok(ReplCommand::Quit),
      _ => {
        return Err("Input error: unknown command.".to_string())
      }
    }
  }

  fn state_init(&mut self, command: &ReplCommand) {
    debug!("State: Init");
    /*
    match self.repl_mailbox.try_recv() {
      Ok(ref msg) => {
        debug!("Received: {:?}", msg);
      }
      Err(sync::mpsc::TryRecvError::Empty) => {
        thread::sleep(Duration::from_millis(500));
      }
      Err(sync::mpsc::TryRecvError::Disconnected) => {
        error!("Receiver disconnected.");
      }
    }
    */
    match command {
      &ReplCommand::Help |
      &ReplCommand::List{ .. } |
      &ReplCommand::WorldGen{ .. } |
      &ReplCommand::Start { .. } |
      &ReplCommand::Load { .. } |
      &ReplCommand::Quit => self.execute_command(command),
      _ => {
        println!("Invalid command. Enter 'help' for valid commands.");
      }
    }
  }
  
  fn state_running(&mut self, command: &ReplCommand) {
    debug!("State: Running");
    match command {
      &ReplCommand::Help |
      &ReplCommand::Run |
      &ReplCommand::Order { .. } |
      &ReplCommand::Pause |
      &ReplCommand::Step { .. } |
      &ReplCommand::Save { .. } |
      &ReplCommand::Stop |
      &ReplCommand::Inspect{ .. } |
      _ => {
        println!("Invalid command. Enter 'help' for valid commands.");
      }
    }
  }

  fn state_generating(&mut self, command: &ReplCommand) {
    debug!("State: Generating");
    match command {
      &ReplCommand::Accept |
      &ReplCommand::Reject => self.execute_command(command),
      _ => {
        println!("Invalid command. Please enter 'accept' or 'reject'.");
      }
    }
  }

  fn state_final(&mut self, command: &ReplCommand) {
    debug!("State: Final");
    match command {
      &ReplCommand::Quit => self.execute_command(command),
      _ => {
        println!("Invalid command.");
      }
    }
  }

  fn execute_command(&mut self, command: &ReplCommand) {
    match command {
      &ReplCommand::Help => {
        let help_text = r#"Commands:
    help [<command>]
    list scenarios
         worlds
         savefiles
    worldgen <name>
    accept
    reject
    start <world> <scenario>
    load <savefile>
    run
    pause
    step [<n>]
    stop
    inspect config
            scenario_config
    quit
        "#;
        println!("{}", help_text);
      }
      &ReplCommand::List{ ref target } => {
        match target.as_str() {
          "scenarios" => {
            for scenario in self.scenario_config.scenarios.iter() {
              println!("{}", scenario.name);
            }
          }
          "worlds" => {
            unimplemented!();
          }
          "savefiles" => {
            unimplemented!();
          }
          &_ => {
            error!("Unexpected list target.");
          }
        }
      }
      &ReplCommand::WorldGen{ ref name } => {
        let msg = engine::EngineMessage::ReqGenerate{ name: name.clone() };
        self.state = ReplState::Generating;
        self.engine_mailbox.send(msg).unwrap();
        match self.repl_mailbox.recv().unwrap() {
          engine::EngineMessage::ResGenerate{ ref env } => {
            println!("World generated. ");
            println!("{:?}", &env);
            println!("accept / reject ?");
            // TODO hold on to env until next loop around
            // self.env = Some(env.clone());
          }
          msg => {
            error!("Unexpected message from engine: {:?}", msg);
          }
        }
      }
      &ReplCommand::Accept => {
        // TODO save generated world
        println!("Saving generated world...");
        self.state = ReplState::Init;
      }
      &ReplCommand::Reject => {
        // TODO save discard generated world
        println!("Discarding generating world...");
        self.state = ReplState::Init;
      }
      &ReplCommand::Start { ref world_name, ref scenario_name } => {
        self.state = ReplState::Running;
        let msg = engine::EngineMessage::MsgStart{
          world_name: world_name.clone(),
          scenario_name: scenario_name.clone()
        };
        self.engine_mailbox.send(msg).unwrap();
      }
      &ReplCommand::Load { ref savefile } => {
        self.state = ReplState::Running;
        let msg = engine::EngineMessage::MsgLoad{ savefile: savefile.clone() };
        self.engine_mailbox.send(msg).unwrap();
      }
      &ReplCommand::Run => {
        let msg = engine::EngineMessage::MsgRun;
        self.engine_mailbox.send(msg).unwrap();
      }
      &ReplCommand::Order { params: ref _params } => {
        unimplemented!();
      }
      &ReplCommand::Pause => {
        let msg = engine::EngineMessage::MsgPause;
        self.engine_mailbox.send(msg).unwrap();
      }
      &ReplCommand::Step { ref steps } => {
        let msg = engine::EngineMessage::MsgStep{ steps: *steps };
        self.engine_mailbox.send(msg).unwrap();
      }
      &ReplCommand::Save { ref savefile } => {
        let msg = engine::EngineMessage::MsgSave{ savefile: savefile.clone() };
        self.engine_mailbox.send(msg).unwrap();
      }
      &ReplCommand::Stop => {
        let msg = engine::EngineMessage::MsgStop;
        self.engine_mailbox.send(msg).unwrap();
      }
      &ReplCommand::Inspect{ ref target } => {
        match target.as_str() {
          "config" => {
            let s = serde_yaml::to_string(&self.config).unwrap();
            println!("{}", s);
          },
          "scenario_config" => {
            let s = serde_yaml::to_string(&self.scenario_config).unwrap();
            println!("{}", s);
          },
          &_ => {
            println!("Unknown inspect target.");
          },
        }
      }
      &ReplCommand::Quit => {
        self.engine_mailbox.send(engine::EngineMessage::MsgQuit).unwrap();
        self.state = ReplState::Final;
      }
    }
  }
}

