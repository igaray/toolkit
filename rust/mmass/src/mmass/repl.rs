use std::io;
use std::io::Write;
use std::io::BufRead;
use std::collections::HashMap;
use std::thread;
use std::sync;
use std::sync::mpsc::channel;
use serde_yaml;

use mmass;

pub enum ReplCommand {
  Quit,
  Help,
  Start {params: HashMap<String, String>},
  WorldGen {params: HashMap<String, String>},
  Inspect {params: HashMap<String, String>},
}

pub enum ReplMessage {
}

pub struct Repl {
  repl_mailbox: sync::mpsc::Receiver<ReplMessage>,
  engine_mailbox: sync::mpsc::Sender<mmass::engine::EngineMessage>,
  config: mmass::config::Config,
  scenario_config: mmass::scenario::ScenarioConfig,
  command_history: Vec<ReplCommand>,
}

impl Repl {
  pub fn new(
      repl_mailbox: sync::mpsc::Receiver<ReplMessage>,
      engine_mailbox: sync::mpsc::Sender<mmass::engine::EngineMessage>,
      config: mmass::config::Config,
      scenario_config: mmass::scenario::ScenarioConfig,
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
          println!("{}", reason);
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
    let mut params = HashMap::new();
    match tokens[0] {
      "quit" => return Ok(ReplCommand::Quit),
      "help" => return Ok(ReplCommand::Help),
      "start" => {
          return Ok(ReplCommand::Start{ params: params })
        },
      "worldgen" => {
          return Ok(ReplCommand::WorldGen{ params: params })
        },
      "inspect" => {
          if 1 < tokens.len() {
            let k = String::from("target");
            let v = String::from(tokens[1]);
            params.insert(k, v);
            return Ok(ReplCommand::Inspect{ params: params })
          } else {
            return Err(String::from("Ërror: inspect needs a parameter."))
          }
        },
      _ => {
          return Err(String::from("Error: Unknown command."))
        }
    }
  }

  fn execute_command(&mut self, command: &ReplCommand) {
    match command {
      &ReplCommand::Quit => {
        self.engine_mailbox.send(mmass::engine::EngineMessage::MsgStop);
      },
      &ReplCommand::Help => {
        println!("Commands:");
        println!("    quit");
        println!("    help");
        println!("    start");
        println!("    worldgen");
        println!("    inspect config");
        println!("            scenarios");
      },
      &ReplCommand::Start{ .. } => {
        println!("Starting...");
      },
      &ReplCommand::WorldGen{ .. }  => {
        println!("Generating world...");
      },
      &ReplCommand::Inspect{ params: ref p, .. } => {
        match p["target"].as_str() {
          "config" => {
            let s = serde_yaml::to_string(&self.config).unwrap();
            println!("{}", s);
          },
          "scenario_config" => {
            let s = serde_yaml::to_string(&self.scenario_config).unwrap();
            println!("{}", s);
          },
          &_ => {
            println!("Error: unknown inspect target");
          },
        }
      },
    }
  }
}

