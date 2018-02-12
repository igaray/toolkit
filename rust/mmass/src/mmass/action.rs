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
  pub fn new() -> Action {
    let action = Action{id: 0, kind: ActionKind::Noop};
    return action
  }
}

pub struct Actions {
  _data: Vec<Action>
}

impl Actions {
  pub fn new() -> Actions {
    return Actions{_data: Vec::new()}
  }

  pub fn add(&self, action: Action) -> Result<(), ()> {
    let res = Ok(());
    return res
  }

  pub fn remove(&self, id: u64) -> Result<(), ()> {
    let res = Ok(());
    return res
  }

  pub fn find(&self, id: u64) -> Result<Action, ()> {
    let action = Action{id: 0, kind: ActionKind::Noop};
    let res = Ok(action);
    return res
  }
}

