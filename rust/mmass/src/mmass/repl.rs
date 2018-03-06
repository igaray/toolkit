use std::io;
use std::io::Write;
use std::io::BufRead;
use std::collections::HashMap;

use mmass;

pub enum ReplCommand {
  Quit,
  Help,
  Start {params: HashMap<String, String>},
  WorldGen {params: HashMap<String, String>},
  Inspect {params: HashMap<String, String>},
}

pub struct Repl {
  command_history: Vec<ReplCommand>,
}

impl Repl {
  pub fn new() -> Repl {
    let repl = Repl{
      command_history: Vec::new(),
      };
    return repl
  }
  
  pub fn run(&mut self) {
    loop {
      let input = Repl::prompt();
      match Repl::parse(input) {
        Ok(command) => {
          match command {
            ReplCommand::Quit => {
              break;
            },
            _ => {
              self.execute_command(&command);
              self.command_history.push(command);
            }
          }
        },
        Err(reason) => {
          println!("{:?}", reason);
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
    let params = HashMap::new();
    match tokens[0] {
      "quit" => {
          return Ok(ReplCommand::Quit)
        },
      "help" => {
          return Ok(ReplCommand::Help)
        },
      "start" => {
          return Ok(ReplCommand::Start{ params: params })
        },
      "worldgen" => {
          return Ok(ReplCommand::WorldGen{ params: params })
        },
      "inspect" => {
          return Ok(ReplCommand::Inspect{ params: params })
        },
      _ => {
          return Err(String::from("Unknown command."))
        }
    }
  }

  fn execute_command(&mut self, command: &ReplCommand) {
    match command {
      &ReplCommand::Quit => {
        // Quit has no implementation because we can never reach here.
      },
      &ReplCommand::Help => {
        println!("Commands:");
        println!("    quit");
        println!("    help");
        println!("    start");
        println!("    worldgen");
        println!("    inspect");
      },
      &ReplCommand::Start{ .. } => {
        println!("Starting...");
      },
      &ReplCommand::WorldGen{ .. }  => {
        println!("Generating world...");
      },
      &ReplCommand::Inspect{ .. } => {
        println!("Inspecting...");
      },
    }
  }
}

