/*
mmass config
- difficulty
- ui language
- autosave enabled
- initial save
- pause on load
- temperature
- weather
- economy
- invaders
*/

use serde_yaml;
use std::env;
use std::fs;
use std::io::Read;

use mmass::agent;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Agent {
    pub kind: agent::AgentKind,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Scenario {
    pub name: String,
    pub starting_units: Option<Vec<Agent>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct Config {
    pub debug: bool,
    pub scenarios: Vec<Scenario>,
}

impl Config {
    pub fn new() -> Config {
        let mut config_path = env::current_dir().unwrap();
        config_path.push("config/config.yaml");

        match fs::File::open(config_path) {
            Ok(mut config_file) => {
                let mut config_file_content = String::new();
                config_file
                    .read_to_string(&mut config_file_content)
                    .unwrap();

                match serde_yaml::from_str(config_file_content.as_str()) {
                    Ok(config) => return config,
                    Err(reason) => {
                        error!("Unable to read configuration file: {:?}", reason);
                        panic!();
                    }
                }
            }
            Err(reason) => {
                error!(
                    "Unable to read configuration file. Loading defaults. Error: {:?}",
                    reason
                );
                // Build default config.
                let a1 = Agent {
                    kind: agent::AgentKind::Reactive,
                };
                let a2 = Agent {
                    kind: agent::AgentKind::Reactive,
                };

                let mut starting_units = Vec::new();
                starting_units.push(a1);
                starting_units.push(a2);

                let sc1 = Scenario {
                    name: String::from("basic"),
                    starting_units: Some(starting_units),
                };

                let sc2 = Scenario {
                    name: String::from("advanced"),
                    starting_units: None,
                };

                let mut scs = Vec::new();
                scs.push(sc1);
                scs.push(sc2);

                let c = Config {
                    debug: true,
                    scenarios: scs,
                };
                let s = serde_yaml::to_string(&c).unwrap();
                println!("{}", s);
                return c;
            }
        }
    }
}
