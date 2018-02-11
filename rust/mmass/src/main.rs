#[macro_use]
extern crate log;
extern crate env_logger;
extern crate rand;

mod mmass;

fn main() {
  env_logger::init();

  let i = rand::random::<u64>();
  print!("random i64: {:?}", i);
  let config = mmass::config::load();
  let scenario = mmass::scenario::load();
  let engine = mmass::engine::new(config, scenario);
  error!("| error: program incomplete");
}
