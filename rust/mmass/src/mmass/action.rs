extern crate rand;

use std::collections::HashMap;

// Actions
pub enum ActionKind {
  Noop,
  Move,
  Reaction
}

pub struct Action {
  id: u64,
  pub kind: ActionKind
}

impl Action {
  pub fn new(kind: ActionKind) -> Action {
    let id = rand::random::<u64>();
    let action = Action{id: id, kind: kind};
    return action
  }
}

pub struct Actions {
  _data: HashMap<u64, Action>
}

impl Actions {
  pub fn new() -> Actions {
    return Actions{_data: HashMap::new()}
  }

  pub fn add(&mut self, action: Action) {
    self._data.insert(action.id, action);
  }

  pub fn remove(&mut self, id: u64) {
    self._data.remove(&id);
  }

  pub fn find(&mut self, id: u64) -> Option<&mut Action> {
    let res = self._data.get_mut(&id);
    return res
  }
}

