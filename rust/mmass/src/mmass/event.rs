use std::collections::HashMap;
use rand;

// Events
pub enum EventKind {
}

pub struct Event {
  id: u64,
  kind: EventKind,
}

impl Event {
  pub fn new(kind: EventKind) -> Event {
    let id = rand::random::<u64>();
    let event = Event{id: id, kind: kind};
    return event
  }
}

pub struct Events {
  data: HashMap<u64, Event>,
}

impl Events {
  pub fn new() -> Events {
    unimplemented!();
  }

  pub fn add(&mut self, _event: Event) {
    unimplemented!();
  }

  pub fn remove(&mut self, _id: u64) {
    unimplemented!();
  }

  pub fn get(&mut self, _id: u64) -> Option<&mut Event> {
    unimplemented!();
  }
}
