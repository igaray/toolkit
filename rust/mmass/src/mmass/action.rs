use std::collections::HashMap;
use rand;

#[derive(Debug, Deserialize, Serialize)]
pub enum ActionKind {
  Noop,
  Move,
  Reaction,
}

#[derive(Debug, Deserialize, Serialize)]
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

#[derive(Debug, Deserialize, Serialize)]
pub struct Actions {
  data: HashMap<u64, Action>
}

impl Actions {
  pub fn new() -> Actions {
    return Actions{data: HashMap::new()}
  }
}

