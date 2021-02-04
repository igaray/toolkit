#[macro_use]
extern crate log;
extern crate env_logger;

use std::{thread, time};

fn main() {
    let one_second = time::Duration::from_millis(1000);
    env_logger::init();
    loop {
        trace!(target: "test", "trace");
        debug!("debug");
        info!("info");
        warn!("warn");
        error!("error");
        thread::sleep(one_second);
    }
}
