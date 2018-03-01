use std::collections::HashMap;
use rand;

// Actions
pub enum ActionKind {
  Noop,
  Move,
  Reaction,
}

pub struct Action {
  pub id: u64,
  pub kind: ActionKind,
  pub params: HashMap<String, String>,
}

impl Action {
  pub fn new(kind: ActionKind) -> Action {
    let id = rand::random::<u64>();
    let action = Action{id: id, kind: kind, params: HashMap::new()};
    return action
  }
}

pub struct Actions {
  data: HashMap<u64, Action>
}

impl Actions {
  pub fn new() -> Actions {
    return Actions{data: HashMap::new()}
  }

  pub fn add(&mut self, action: Action) {
    self.data.insert(action.id, action);
  }

  pub fn remove(&mut self, id: u64) {
    self.data.remove(&id);
  }

  pub fn get(&mut self, id: u64) -> Option<&mut Action> {
    let res = self.data.get_mut(&id);
    return res
  }
}

