use std::env;
use std::fs;
use std::io::Read;
use toml;

use mmass;

// Scenario Configuration
#[derive(Debug, Deserialize)]
pub struct ScenarioConfig {
  scenarios: Vec<Scenario>
}

#[derive(Debug, Deserialize)]
pub struct Scenario {
  name: String,
  global_env_kind: mmass::global_env::GlobalEnvKind,
  local_env_kind: mmass::local_env::LocalEnvMapKind,
}
  
impl Scenario {
  pub fn new() -> ScenarioConfig {
    let mut scenario_path = env::current_dir().unwrap();
    scenario_path.push("config/scenario.toml");

    let mut scenario_file = fs::File::open(scenario_path).unwrap();
    let mut scenario_file_content = String::new();
    scenario_file.read_to_string(&mut scenario_file_content).unwrap();

    let scenario_config: ScenarioConfig = toml::from_str(scenario_file_content.as_str()).unwrap();
    return scenario_config
  }
}
