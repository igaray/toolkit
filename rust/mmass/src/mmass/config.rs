use std::env;
use std::fs;
use std::io::Read;
use std::path;
use toml;

// Engine Configuration
#[derive(Deserialize)]
#[derive(Debug)]
pub struct Config {
  debug: bool
}

impl Config {
  pub fn new() -> Config {
    let mut config_path = env::current_dir().unwrap();
    config_path.push("config/config.toml");

    let mut config_file = fs::File::open(config_path).unwrap();
    let mut config_file_content = String::new();
    config_file.read_to_string(&mut config_file_content).unwrap();

    let mut config: Config  = toml::from_str(config_file_content.as_str()).unwrap();
    println!("config: {:?}", config);
    return config
  }
}

