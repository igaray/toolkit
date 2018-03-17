use std::io;
use std::io::Write;
use std::io::BufRead;
use std::collections::HashMap;
use std::thread;
use std::sync;
use std::env;
use std::fs;

use serde_yaml;
use bincode;

use mmass::config as config;
use mmass::scenario as scenario;
use mmass::engine as engine;
use mmass::local_env as local_env;

pub enum ReplState {
  Main,
  Generating,
  Running,
  Paused,
  Final,
}

pub enum ReplCommand {
  Help,
  List{ target: String },
  WorldGen { name: String },
  Accept,
  Reject,
  Start { scenario_name: String, world_name: String },
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
  env: Option<Box<local_env::LocalEnv>>,
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
      state: ReplState::Main,
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
      let input = prompt();
      match parse(input) {
        Ok(ReplCommand::Quit) => {
          self.state_final(ReplCommand::Quit);
          break;
        }
        Ok(command) => {
          match self.state {
            ReplState::Main => self.state_init(command),
            ReplState::Generating => self.state_generating(command),
            ReplState::Running => self.state_running(command),
            ReplState::Paused => self.state_paused(command),
            ReplState::Final => {}
          }
        }
        Err(reason) => println!("Error: {}", reason),
      }
    }
  }

  fn state_init(&mut self, command: ReplCommand) {
    debug!("State: Main");
    match command {
      ReplCommand::Help => self.execute_command_help_init(),
      ReplCommand::List{ target } => self.execute_command_list(target),
      ReplCommand::WorldGen{name } => self.execute_command_worldgen(name),
      ReplCommand::Start { scenario_name, world_name } => self.execute_command_start(scenario_name, world_name),
      ReplCommand::Load { savefile } => self.execute_command_load(savefile),
      ReplCommand::Quit => self.execute_command_quit(),
      _ => {
        println!("Invalid command. Enter 'help' for valid commands.");
      }
    }
  }

  fn state_generating(&mut self, command: ReplCommand) {
    debug!("State: Generating");
    match command {
      ReplCommand::Help => self.execute_command_help_generating(),
      ReplCommand::Accept => self.execute_command_accept(),
      ReplCommand::Reject => self.execute_command_reject(),
      _ => {
        println!("Invalid command. Please enter 'accept' or 'reject'.");
      }
    }
  }

  fn state_running(&mut self, command: ReplCommand) {
    debug!("State: Running");
    match command {
      ReplCommand::Help => self.execute_command_help_running(),
      ReplCommand::Run => self.execute_command_run(),
      ReplCommand::Order { params: _params } => unimplemented!(),
      ReplCommand::Pause => self.execute_command_pause(),
      ReplCommand::Step { steps } => self.execute_command_step(steps),
      ReplCommand::Save { savefile } => self.execute_command_save(savefile),
      ReplCommand::Stop => self.execute_command_stop(),
      ReplCommand::Inspect{ target } => self.execute_command_inspect(target),
      _ => {
        println!("Invalid command. Enter 'help' for valid commands.");
      }
    }
  }

  fn state_paused(&mut self, command: ReplCommand) {
    debug!("State: Paused");
    match command {
      ReplCommand::Help => self.execute_command_help_paused(),
      _ => println!("Invalid command. Enter 'help' for valid commands.")

    }
  }

  fn state_final(&mut self, command: ReplCommand) {
    debug!("State: Final");
    match command {
      ReplCommand::Quit => self.execute_command_quit(),
      _ => {
        println!("Invalid command.");
      }
    }
  }

  fn execute_command_help_init(&mut self) {
    let help_text = r#"Commands:
    help
    list scenarios
         worlds
         saves
    worldgen <name>
    start <scenario> <world>
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

  fn execute_command_help_generating(&mut self) {
    let help_text = r#"Commands:
    help
    accept
    reject
    quit
    "#;
    println!("{}", help_text);
  }

  fn execute_command_help_running(&mut self) {
    let help_text = r#"Commands:
    help
    pause
    stop
    quit
    "#;
    println!("{}", help_text);
  }

  fn execute_command_help_paused(&mut self) {
    let help_text = r#"Commands:
    help
    run
    step [<n>]
    stop
    inspect config
            scenario_config
    quit
    "#;
    println!("{}", help_text);
  }

  fn execute_command_list(&mut self, target: String) {
    match target.as_str() {
      "scenarios" => {
        for scenario in self.scenario_config.scenarios.iter() {
          println!("{}", scenario.name);
        }
      }
      "worlds" => {
        list_files("./worlds");
      }
      "saves" => {
        list_files("./saves");
      }
      &_ => {
        error!("Unexpected list target.");
      }
    }
  }

  fn execute_command_worldgen(&mut self, name: String) {
    let req = engine::EngineMessage::ReqGenerate{ name: name.clone() };
    self.engine_mailbox.send(req).unwrap();
    match self.repl_mailbox.recv().unwrap() {
      engine::EngineMessage::ResGenerate{ env } => {
        println!("World generated. accept / reject ?");
        self.state = ReplState::Generating;
        self.env = Some(env);
      }
      msg => {
        error!("Unexpected message from engine: {:?}", msg);
        panic!();
      }
    }
    self.state = ReplState::Generating;
  }

  fn execute_command_accept(&mut self) {
    match self.env {
      Some(ref env) => {
        let serialized_world: Vec<u8> = bincode::serialize(&*env).unwrap();
        let mut save_path = env::current_dir().unwrap();
        save_path.push("worlds");
        save_path.push(&env.name);
        save_path.set_extension("bin");
        println!("Saving generated world to {:?}", &save_path);
        let mut save_file = fs::File::create(save_path).unwrap();
        save_file.write_all(&serialized_world).unwrap();
      }
      None => {
        error!("Trying to serialize environment but there is None.");
        panic!();
      }
    }
    self.state = ReplState::Main;
  }

  fn execute_command_reject(&mut self) {
    println!("Discarding generating world...");
    self.env = None;
    self.state = ReplState::Main;
  }

  fn execute_command_start(&mut self, scenario_name: String, world_name: String) {
    let msg = engine::EngineMessage::MsgStart{
      world_name: world_name.clone(),
      scenario_name: scenario_name.clone()
    };
    self.engine_mailbox.send(msg).unwrap();
    self.state = ReplState::Running;
  }

  fn execute_command_load(&mut self, savefile: String) {
    let msg = engine::EngineMessage::MsgLoad{ savefile: savefile.clone() };
    self.engine_mailbox.send(msg).unwrap();
    self.state = ReplState::Running;
  }

  fn execute_command_run(&mut self) {
    let msg = engine::EngineMessage::MsgRun;
    self.engine_mailbox.send(msg).unwrap();
    self.state = ReplState::Running;
  }

  fn execute_command_pause(&mut self) {
    let msg = engine::EngineMessage::MsgPause;
    self.engine_mailbox.send(msg).unwrap();
    self.state = ReplState::Paused;
  }

  fn execute_command_save(&mut self, savefile: String) {
    let msg = engine::EngineMessage::MsgSave{ savefile: savefile.clone() };
    self.engine_mailbox.send(msg).unwrap();
  }

  fn execute_command_step(&mut self, steps: u8) {
    let msg = engine::EngineMessage::MsgStep{ steps };
    self.engine_mailbox.send(msg).unwrap();
  }

  fn execute_command_stop(&mut self) {
    let msg = engine::EngineMessage::MsgStop;
    self.engine_mailbox.send(msg).unwrap();
    self.state = ReplState::Main;
  }

  fn execute_command_inspect(&mut self, target: String) {
    match target.as_str() {
      "config" =>
        println!("{}", serde_yaml::to_string(&self.config).unwrap()),
      "scenario_config" =>
        println!("{}", serde_yaml::to_string(&self.scenario_config).unwrap()),
      &_ =>
        println!("Unknown inspect target.")
    }
  }

  fn execute_command_quit(&mut self) {
    self.engine_mailbox.send(engine::EngineMessage::MsgQuit).unwrap();
    self.state = ReplState::Final;
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

fn list_files(directory: &str) {
  let paths = fs::read_dir(directory).unwrap();
  for direntry in paths {
    let direntry_value = direntry.unwrap();
    let path = direntry_value.path();
    let file_name = path.file_stem().unwrap();
    let file_name_str = file_name.to_str().unwrap();
    println!("{}", file_name_str);
  }
}
