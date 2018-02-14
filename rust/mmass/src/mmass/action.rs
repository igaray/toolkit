use std::collections::HashMap;

// Actions
pub enum ActionKind {
  Noop,
  Move,
  Reaction
}

pub struct Action {
  id: u64,
  kind: ActionKind
}

impl Action {
  pub fn new(id: u64, kind: ActionKind) -> Action {
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

  pub fn find(&self, id: u64) -> Option<&Action> {
    let res = self._data.get(&id);
    return res
  }
}

