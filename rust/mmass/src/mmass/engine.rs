// Engine
use std::thread;
use std::time::Duration;
use std::sync;
use std::sync::mpsc::channel;

use mmass::config as config;
use mmass::scenario as scenario;
use mmass::global_env as global_env;
use mmass::local_env as local_env;

enum EngineState {
  Init,
  Final
}

pub struct Engine {
  mailbox: sync::mpsc::Receiver<u32>,
  config: config::Config,
  scenario: scenario::ScenarioConfig,
  state: EngineState,
  global_env: global_env::GlobalEnv,
  local_envs: Vec<local_env::LocalEnv>
}

impl Engine {
  pub fn new(config: config::Config, scenario: scenario::ScenarioConfig) -> (sync::mpsc::Sender<u32>, thread::JoinHandle<u32>) {
    let (sender, receiver) = channel::<u32>();
    let mut engine = Engine{
      mailbox: receiver,
      config: config,
      scenario: scenario,
      state: EngineState::Init,
      global_env: global_env::new(global_env::GlobalEnvKind::SquareGrid),
      local_envs: Vec::new()
      };
    let handle = thread::spawn(move || { engine.run(); 0 });
    return (sender, handle)
  }

  pub fn run(&mut self) {
    let mut i = 30;
    loop {
      match self.state {
        EngineState::Init => {
          if i == 0 {
            println!("Ëngine reached final state.");
            self.state = EngineState::Final;
          }
          else {
            thread::sleep(Duration::from_millis(500));
            i -= 1;
          }
        }
        EngineState::Final => {
          println!("Ëngine exiting...");
          break;
        }
      }
    }
  }
}


