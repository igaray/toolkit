use std::collections::HashMap;
use rand;

use mmass::local_env as local_env;
use mmass::action as action;
use mmass::percept as percept;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum AgentKind {
  Joystick,
  Reactive
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum AgentData {
  Joystick {
  },
  Reactive {
  }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Agent {
  pub id: u64,
  pub position: local_env::Position,
  pub data: AgentData,
}

impl Agent {
  pub fn new(kind: &AgentKind, position: local_env::Position) -> Agent {
    let id = rand::random::<u64>();
    match kind {
      &AgentKind::Joystick => {
        return Agent{
          id: id,
          position: position,
          data: AgentData::Joystick{  }
        }
      }
      &AgentKind::Reactive => {
        return Agent{
          id: id,
          position: position,
          data: AgentData::Reactive{  }
        }
      }
    }
  }

  pub fn action(&self) -> action::Action {
    // TODO Make action function and percept constructor take a reference to the world state and the agent to figure out what it perceives.
    let _percept = percept::Percept::new();
    return action::Action::new(self.id, action::ActionData::Noop)
  }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Agents {
  pub data: HashMap<u64, Agent>
}

impl Agents {
  pub fn new() -> Agents {
    return Agents{data: HashMap::new()}
  }

  pub fn add(&mut self, agent: Agent) {
    self.data.insert(agent.id, agent);
  }
}
