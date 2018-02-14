#[macro_use]
extern crate log;
extern crate env_logger;
extern crate rand;

mod mmass;

fn main() {
  env_logger::init();

  let i = rand::random::<u64>();
  println!("random i64: {:?}", i);

  let config = mmass::config::Config::new();
  config.load();

  let scenario = mmass::scenario::Scenario::new();
  scenario.load();

  let engine = mmass::engine::new(config, scenario);
  error!("| error: program incomplete");
}
