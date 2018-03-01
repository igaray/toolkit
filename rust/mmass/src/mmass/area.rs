use std::collections::HashMap;
use rand;

// Areas

pub struct Area {
  id: u64,
}

impl Area {
  pub fn new() -> Area {
    let id = rand::random::<u64>();
    let area = Area{id: id};
    return area
  }
}

pub struct Areas {
  data: HashMap<u64, Area>,
}

impl Areas {
  pub fn new() -> Areas {
    unimplemented!();
  }

  pub fn add(&mut self, _area: Area) {
    unimplemented!();
  }

  pub fn remove(&mut self, _id: u64) {
    unimplemented!();
  }

  pub fn get(&mut self, _id: u64) -> Option<&mut Area> {
    unimplemented!();
  }
}


