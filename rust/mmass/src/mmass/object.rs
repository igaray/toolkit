use std::collections::HashMap;
use rand;

// Objects
pub struct Object {
  id: u64
}

impl Object {
  pub fn new() -> Object {
    let id = rand::random::<u64>();
    return Object{id: id}
  }
}

pub struct Objects {
  data: HashMap<u64, Object>
}

impl Objects {
  pub fn new() -> Objects {
    unimplemented!();
  }

  pub fn add(&mut self, _object: Object) {
    unimplemented!();
  }

  pub fn remove(&mut self, _id: u64) {
    unimplemented!();
  }

  pub fn get(&mut self, _id: u64) -> Option<&mut Object> {
    unimplemented!();
  }
}

