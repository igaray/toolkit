use std::env;
use std::fs;
use std::io::Read;
use serde_yaml;

// Engine Configuration
#[derive(Debug, Deserialize)]
pub struct Config {
  debug: bool
}

impl Config {
  pub fn new() -> Config {
    let mut config_path = env::current_dir().unwrap();
    config_path.push("config/config.yaml");

    let mut config_file = fs::File::open(config_path).unwrap();
    let mut config_file_content = String::new();
    config_file.read_to_string(&mut config_file_content).unwrap();

    let config: Config  = serde_yaml::from_str(config_file_content.as_str()).unwrap();
    return config
  }
}

