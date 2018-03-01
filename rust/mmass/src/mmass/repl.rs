use std::io;
use std::io::Write;
use std::io::BufRead;
use std::collections::HashMap;

use mmass;

pub enum ReplCommandKind {
  Quit,
  Start,
  WorldGen,
  Inspect
}

pub struct ReplCommand{
  pub kind: ReplCommandKind,
  pub params: HashMap<String, String>,
}

pub struct Repl {
  config: mmass::config::Config,
  scenario_config: mmass::scenario::ScenarioConfig,
  command_history: Vec<ReplCommand>,
}

impl Repl {
  pub fn new(config: mmass::config::Config, scenario_config: mmass::scenario::ScenarioConfig) -> Repl {
    let repl = Repl{
      config: config,
      scenario_config: scenario_config,
      command_history: Vec::new(),
      };
    return repl
  }
  
  pub fn run(&mut self) {
    loop {
      let input = Repl::prompt();
      match Repl::parse(input) {
        Ok(command) => {
          match command.kind {
            ReplCommandKind::Quit => {
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
    stdout.flush().expect("Error flushing stdout");
    let mut input = String::new();
    stdin.lock().read_line(&mut input).expect("Error reading from stdin");
    return input
  }

  fn parse(input: String) -> Result<ReplCommand, String> {
    let tokens: Vec<&str> = input.trim().split(' ').collect();
    let params = HashMap::new();
    match tokens[0] {
      "quit" => {
          return Ok(ReplCommand{kind: ReplCommandKind::Quit, params: params})
        },
      "start" => {
          return Ok(ReplCommand{kind: ReplCommandKind::Start, params: params})
        },
      "worldgen" => {
          return Ok(ReplCommand{kind: ReplCommandKind::WorldGen, params: params})
        },
      "inspect" => {
          return Ok(ReplCommand{kind: ReplCommandKind::Inspect, params: params})
        },
      _ => {
          return Err(String::from("Unknown command."))
        }
    }
  }

  fn execute_command(&mut self, command: &ReplCommand) {
    match command.kind {
      ReplCommandKind::Quit => {
        // Quit has no implementation because we can never reach here.
      },
      ReplCommandKind::Start => {
        println!("Starting...");
      },
      ReplCommandKind::WorldGen => {
        println!("Generating world...");
      },
      ReplCommandKind::Inspect => {
        println!("Inspecting...");
      },
    }
  }
}

