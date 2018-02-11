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
  scenario: scenario::Scenario,
  state: EngineState,
  global_env: global_env::GlobalEnv,
  local_envs: Vec<local_env::LocalEnv>
}

pub fn new(config: config::Config, scenario: scenario::Scenario) -> Engine {
  return Engine{
    config: config,
    scenario: scenario,
    state: EngineState::Init,
    global_env: global_env::new(),
    local_envs: Vec::new()
    }
}

