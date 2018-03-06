// Engine
use mmass::config as config;
use mmass::scenario as scenario;
use mmass::global_env as global_env;
use mmass::local_env as local_env;

enum EngineState {
  Init,
  Final
}

pub struct Engine {
  config: config::Config,
  scenario: scenario::ScenarioConfig,
  state: EngineState,
  global_env: global_env::GlobalEnv,
  local_envs: Vec<local_env::LocalEnv>
}

impl Engine {
  pub fn new(config: config::Config, scenario: scenario::ScenarioConfig) -> Engine {
    return Engine{
      config: config,
      scenario: scenario,
      state: EngineState::Init,
      global_env: global_env::new(global_env::GlobalEnvKind::SquareGrid),
      local_envs: Vec::new()
      }
  }

  pub fn run(&mut self) {
    loop {
      match self.state {
        EngineState::Init => {
          self.state = EngineState::Final;
        }
        EngineState::Final => {
          break;
        }
      }
    }
  }
}


