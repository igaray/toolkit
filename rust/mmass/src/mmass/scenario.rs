use std::env;
use std::fs;
use std::io::Read;
use std::path;
use toml;

use mmass;

// Scenario Configuration
#[derive(Deserialize)]
#[derive(Debug)]
pub struct Scenario {
  global_env_kind: mmass::global_env::GlobalEnvKind,
  local_env_kind: mmass::local_env::LocalEnvKind
}

impl Scenario {
  pub fn new() -> Scenario {
    let mut scenario_path = env::current_dir().unwrap();
    scenario_path.push("config/scenario.toml");

    let mut scenario_file = fs::File::open(scenario_path).unwrap();
    let mut scenario_file_content = String::new();
    scenario_file.read_to_string(&mut scenario_file_content).unwrap();

    let mut scenario: Scenario = toml::from_str(scenario_file_content.as_str()).unwrap();
    println!("scenario: {:?}", scenario);
    return scenario
  }
}

