use std::env;
use std::fs;
use std::io::Read;
use serde_yaml;

use mmass::agent as agent;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Scenario {
  pub name: String,
  pub starting_units: Option<Vec<agent::Agent>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Config {
  pub debug: bool,
  pub scenarios: Vec<Scenario>,
}

impl Config {
  pub fn new() -> Config {
    /*
    let a1 = mmass::agent::Agent::new(mmass::agent::AgentKind::Reactive, String::from("a1"));
    let a2 = mmass::agent::Agent::new(mmass::agent::AgentKind::Reactive, String::from("a2"));
    let mut units = Vec::new();
    units.push(a1);
    units.push(a2);
    let sc1 = Scenario{
      name: String::from("basic"),
      global_env_kind: mmass::global_env::GlobalEnvKind::SquareGrid,
      local_env_kind: mmass::local_env::LocalEnvMapKind::SquareGrid,
      starting_units: Some(units),
    };
    let sc2 = Scenario{
      name: String::from("advanced"),
      global_env_kind: mmass::global_env::GlobalEnvKind::SquareGrid,
      local_env_kind: mmass::local_env::LocalEnvMapKind::SquareGrid,
      starting_units: None,
      };
    let mut scs = Vec::new();
    scs.push(sc1);
    scs.push(sc2);
    let sc = ScenarioConfig{scenarios: scs};
    let s = serde_yaml::to_string(&sc).unwrap();
    println!("{}", s);
    return ScenarioConfig{scenarios: Vec::new()}
    */

    let mut config_path = env::current_dir().unwrap();
    config_path.push("config/config.yaml");

    let mut config_file = fs::File::open(config_path).unwrap();
    let mut config_file_content = String::new();
    config_file.read_to_string(&mut config_file_content).unwrap();

    let config: Config  = serde_yaml::from_str(config_file_content.as_str()).unwrap();
    return config
  }
}
