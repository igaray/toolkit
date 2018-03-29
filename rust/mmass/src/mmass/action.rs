use std::collections::HashMap;
use rand;

use mmass::local_env as local_env;

#[derive(Debug, Deserialize, Serialize)]
pub enum ActionKind {
  Noop,
  Move,
}

#[derive(Debug, Deserialize, Serialize)]
pub enum ActionData {
  Noop,
  Move{ direction: local_env::Direction },
  GoTo{ position: local_env::Position },
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Action {
  pub id: u64,
  pub agent_id: u64,
  pub data: ActionData,
}

impl Action {
  pub fn new(agent_id: u64, data: ActionData) -> Action {
    let id = rand::random::<u64>();
    let action = Action{
      id: id,
      agent_id: agent_id,
      data: data,
    };
    return action
  }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Actions {
  pub data: HashMap<u64, Action>
}

impl Actions {
  pub fn new() -> Actions {
    return Actions{data: HashMap::new()}
  }

  pub fn clear(&mut self) {
    self.data.drain();
  }

  pub fn add(&mut self, agent_id: u64, action: Action) {
    self.data.insert(agent_id, action);
  }
}
