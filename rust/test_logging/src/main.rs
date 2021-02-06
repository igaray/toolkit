#[macro_use]
extern crate log;
extern crate env_logger;

use std::{thread, time};
pub mod sort {
    pub fn insertion(a: &mut [usize; 10]) {
        let n = 10;
        let mut i = 1;
        while i < n {
          let mut j = i;
          while j > 0 && a[j-1] > a[j] {
            let t = a[j-1];
            a[j-1] = a[j];
            a[j] = t;
            j -= 1;
            debug!("a: {:?}", a);
          }
          i += 1;
        }
      }
}

  fn test() {
    let mut a = [2,6,4,7,5,8,3,9,1,0];
    sort::insertion(&mut a);
    debug!("a: {:?}", a);
  }
  
fn main() {
    let one_second = time::Duration::from_millis(1000);
    env_logger::init();
    test();
    loop {
        trace!(target: "test", "trace");
        debug!("debug");
        info!("info");
        warn!("warn");
        error!("error");
        thread::sleep(one_second);
    }
}
