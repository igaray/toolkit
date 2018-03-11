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

pub enum ReplCommand {
  Help,
  List{ target: String },
  WorldGen { name: String },
  Accept,
  Reject,
  Start { scenario_name: String },
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
  config: config::Config,
  scenario_config: scenario::ScenarioConfig,
  command_history: Vec<ReplCommand>,
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
      config: config,
      scenario_config: scenario_config,
      command_history: Vec::new(),
      };
    let handle = thread::spawn(move || { repl.run(); 0 });
    return handle
  }
  
  fn run(&mut self) {
    loop {
      match self.repl_mailbox.try_recv() {
        Ok(ref msg) => {
          debug!("REPL received: {:?}", msg);
        },
        Err(sync::mpsc::TryRecvError::Empty) => {
        },
        Err(sync::mpsc::TryRecvError::Disconnected) => {
          error!("REPL received disconnected.");
        }
      }
      
      let input = Repl::prompt();
      match Repl::parse(input) {
        Ok(command) => {
          match command {
            ReplCommand::Quit => {
              self.execute_command(&command);
              break;
            },
            _ => {
              self.execute_command(&command);
              self.command_history.push(command);
            }
          }
        },
        Err(reason) => {
          error!("{}", reason);
        }
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
            let target = String::from(tokens[1]);
            return Ok(ReplCommand::List{ target })
          } else {
            return Err(String::from("Input error: list needs a parameter."))
          }
        },
      "worldgen" => {
          if 1 < tokens.len() {
            let name = String::from(tokens[1]);
            return Ok(ReplCommand::WorldGen{ name })
          } else {
            return Err(String::from("Input error: inspect needs a parameter."))
          }
        },
      "accept" => return Ok(ReplCommand::Accept),
      "reject" => return Ok(ReplCommand::Reject),
      "start" => {
          let scenario_name = String::from(tokens[1]);
          return Ok(ReplCommand::Start{ scenario_name: scenario_name })
        }, 
      "load" => {
          let savefile = String::from(tokens[1]);
          return Ok(ReplCommand::Load{ savefile })
        },
      "run" => return Ok(ReplCommand::Run),
      "order" => {
          // TODO
          let params = HashMap::new();
          return Ok(ReplCommand::Order{ params })
        },
      "pause" => return Ok(ReplCommand::Pause),
      "step" => {
          // TODO
          let steps = 1;
          return Ok(ReplCommand::Step{ steps } )
        },
      "save" => {
          let savefile = String::from(tokens[1]);
          return Ok(ReplCommand::Save{ savefile })
        },
      "stop" => return Ok(ReplCommand::Stop),
      "inspect" => {
          if 1 < tokens.len() {
            let target  = String::from(tokens[1]);
            return Ok(ReplCommand::Inspect{ target: target })
          } else {
            return Err(String::from("Input error: inspect needs a parameter."))
          }
        },
      "quit" => return Ok(ReplCommand::Quit),
      _ => {
          return Err(String::from("Input error: unknown command."))
        }
    }
  }

  fn execute_command(&mut self, command: &ReplCommand) {
    match command {
      &ReplCommand::Help => {
        println!("Commands:");
        println!("    help [<command>]");
        println!("    worldgen <name>");
        println!("    accept");
        println!("    reject");
        println!("    start <world> <scenario>");
        println!("    load <savefile>");
        println!("    run");
        println!("    pause");
        println!("    order ...");
        println!("    save <savefile>");
        println!("    step <n>");
        println!("    stop");
        println!("    list scenarios");
        println!("         savefiles");
        println!("    inspect config");
        println!("            scenario_config");
        println!("            engine");
        println!("            agents");
        println!("            orders");
        println!("    quit");
      },
      &ReplCommand::List{ ref target }  => {
        match target.as_str() {
          "scenarios" => {
            for scenario in self.scenario_config.scenarios.iter() {
              println!("- {}", scenario.name);
            }
          }
          "savefiles" => {
            unimplemented!();
          }
          &_ => {
            error!("Unexpected list target.");
          }
        }
      },
      &ReplCommand::WorldGen{ ref name }  => {
        let msg = engine::EngineMessage::ReqGenerate{ name: name.clone() };
        self.engine_mailbox.send(msg).unwrap();
        match self.repl_mailbox.recv().unwrap() {
          engine::EngineMessage::ResGenerate => {
            println!("World generated. accept / reject ?");
          },
          msg => {
            error!("Unexpected message from engine: {:?}", msg);
          }
        }
      },
      &ReplCommand::Accept => {
        let msg = engine::EngineMessage::MsgAccept;
        self.engine_mailbox.send(msg).unwrap();
      },
      &ReplCommand::Reject => {
        let msg = engine::EngineMessage::MsgReject;
        self.engine_mailbox.send(msg).unwrap();
      },
      &ReplCommand::Start { scenario_name: ref _scenario_name } => {
        unimplemented!();
      },
      &ReplCommand::Load { savefile: ref _savefile } => {
        unimplemented!();
      },
      &ReplCommand::Run => {
        let msg = engine::EngineMessage::MsgRun;
        self.engine_mailbox.send(msg).unwrap();
      },
      &ReplCommand::Order { params: ref _params } => {
        unimplemented!();
      },
      &ReplCommand::Pause => {
        let msg = engine::EngineMessage::MsgPause;
        self.engine_mailbox.send(msg).unwrap();
      },
      &ReplCommand::Step { steps: ref _steps } => {
        unimplemented!();
      },
      &ReplCommand::Save { savefile: ref _savefile } => {
        unimplemented!();
      },
      &ReplCommand::Stop => {
        self.engine_mailbox.send(engine::EngineMessage::MsgStop).unwrap();
      },
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
      },
      &ReplCommand::Quit => {
        self.engine_mailbox.send(engine::EngineMessage::MsgQuit).unwrap();
      },
    }
  }
}

