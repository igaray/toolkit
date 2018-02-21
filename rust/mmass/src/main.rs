#[macro_use]
extern crate log;
extern crate env_logger;
extern crate rand;
#[macro_use]
extern crate serde_derive;
extern crate toml;

mod mmass;

fn main() {
  env_logger::init();

  /*
  let mut actions = mmass::action::Actions::new();
  let mut a1 = mmass::action::Action::new(mmass::action::ActionKind::Noop);
  let a2 = mmass::action::Action::new(mmass::action::ActionKind::Noop);

  // the add copies the action struct into the actions store
  actions.add(a1);
  actions.add(a2);
  // we modify our action a1
  a1.kind = mmass::action::ActionKind::Move;
  // get the 'same' one from the actions store
  let a3 = actions.find(1).unwrap();
  // but the 3 we get back is a different one thn a1
  match a3.kind {
    mmass::action::ActionKind::Noop => { println!("Noop"); },
    mmass::action::ActionKind::Move => { println!("Move"); },
    mmass::action::ActionKind::Reaction => { println!("Reaction"); }
  }

  let i = rand::random::<u64>();
  println!("random i64: {:?}", i);
  */

  let config = mmass::config::Config::new();
  let scenario = mmass::scenario::Scenario::new();
  let engine = mmass::engine::new(config, scenario);

  mmass::repl::run();
  error!("| error: program incomplete");
}
