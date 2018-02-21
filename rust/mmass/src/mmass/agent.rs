extern crate rand;

use std::collections::HashMap;

// Agents
pub enum AgentKind {
  Joystick,
  Reactive,
  StateBased,
  BDI,
  Strategic
}

pub struct Agent {
  kind: AgentKind,
  id: u64
}

impl Agent {
  pub fn new(kind: AgentKind) -> Agent {
    let id = rand::random::<u64>();
    return Agent{id: id, kind: kind}
  }
}

pub struct Agents {
  _data: HashMap<u64, Agent>
}

impl Agents {
  pub fn new() -> Agents {
    return Agents{_data: HashMap::new()}
  }

  pub fn add(&mut self, agent: Agent) {
    self._data.insert(agent.id, agent);
  }

  pub fn remove(&mut self, id: u64) {
    self._data.remove(&id);
  }

  pub fn find(&mut self, id: u64) -> Option<&mut Agent> {
    let res = self._data.get_mut(&id);
    return res
  }
}
