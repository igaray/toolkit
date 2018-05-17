#[macro_use]
extern crate log;
extern crate env_logger;
extern crate rand;
#[macro_use]
extern crate serde_derive;
extern crate bincode;
extern crate serde_yaml;

use std::fs;
use std::sync;

mod mmass;

fn main() {
    env_logger::init();
    init_directories();

    let config = mmass::config::Config::new();

    let (repl_sender, repl_receiver) = sync::mpsc::channel::<mmass::engine::EngineMessage>();
    let (engine_sender, engine_receiver) = sync::mpsc::channel::<mmass::engine::EngineMessage>();

    let engine_thread_handle =
        mmass::engine::Engine::new(engine_receiver, repl_sender, config.clone());
    let repl_thread_handle = mmass::repl::Repl::new(repl_receiver, engine_sender, config.clone());

    engine_thread_handle.join().unwrap();
    repl_thread_handle.join().unwrap();

    error!("Error: program incomplete");
}

fn init_directories() {
    let config_path = "./config";
    let worlds_path = "./worlds";
    let saves_path = "./saves";
    let mut builder = fs::DirBuilder::new();
    builder.recursive(true);
    builder.create(config_path).unwrap();
    builder.create(worlds_path).unwrap();
    builder.create(saves_path).unwrap();
}
