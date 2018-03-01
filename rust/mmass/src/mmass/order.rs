use std::collections::HashMap;
use rand;

// Orders

pub enum OrderKind {
  Noop,
}

pub struct Order {
  id: u64,
  kind: OrderKind,
}

impl Order {
  pub fn new(kind: OrderKind) -> Order {
    let id = rand::random::<u64>();
    let order = Order{id: id, kind: kind};
    return order
  }
}

pub struct Orders {
  data: HashMap<u64, Order>,
}

impl Orders {
  pub fn new() -> Orders {
    unimplemented!();
  }

  pub fn add(&mut self, _order: Order) {
    unimplemented!();
  }

  pub fn remove(&mut self, _id: u64) {
    unimplemented!();
  }

  pub fn get(&mut self, _id: u64) -> Option<&mut Order> {
    unimplemented!();
  }
}
