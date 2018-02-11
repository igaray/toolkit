extern crate toml;

// Engine Configuration
pub struct Config {
}

pub fn new() -> Config {
  return Config{}
}

pub fn load() -> Config {
  let config = new();
  return config
}

