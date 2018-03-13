use std::env;
use std::fs;
use std::io::Read;
use serde_yaml;

use mmass;

// Scenario Configuration
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct ScenarioConfig {
  pub scenarios: Vec<Scenario>
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Scenario {
  pub name: String,
  pub height: u8,
  pub width: u8,
  pub starting_units: Option<Vec<mmass::agent::Agent>>,
}
  
impl Scenario {
  pub fn new() -> ScenarioConfig {
    let mut scenario_path = env::current_dir().unwrap();
    scenario_path.push("config/scenario.yaml");

    let mut scenario_file = fs::File::open(scenario_path).unwrap();
    let mut scenario_file_content = String::new();
    scenario_file.read_to_string(&mut scenario_file_content).unwrap();

    let scenario_config: ScenarioConfig = serde_yaml::from_str(scenario_file_content.as_str()).unwrap();
    return scenario_config

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
  }
}

