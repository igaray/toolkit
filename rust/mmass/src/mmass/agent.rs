use std::collections::HashMap;
use rand;

// Agents

#[derive(Debug)]
pub struct JoystickAgent {
  id: u64,
}

#[derive(Debug)]
pub struct ReactiveAgent {
  id: u64,
}

#[derive(Debug)]
pub struct BDIAgent {
  id: u64,
}

#[derive(Debug)]
pub struct StrategicAgent {
  id: u64,
}

#[derive(Debug, Deserialize)]
pub enum AgentKind {
  Joystick,
  Reactive,
  BDI,
  Strategic,
}

#[derive(Debug)]
pub enum Agent {
  Joystick(JoystickAgent),
  Reactive(ReactiveAgent),
  BDI(BDIAgent),
  Strategic(StrategicAgent),
}

impl Agent {
  pub fn new(kind: AgentKind) -> Agent {
    let id = rand::random::<u64>();
    match kind {
      AgentKind::Joystick => {
          return Agent::Joystick(JoystickAgent{id: id})
        },
      AgentKind::Reactive => {
          return Agent::Reactive(ReactiveAgent{id: id})
        },
      AgentKind::BDI => {
          return Agent::BDI(BDIAgent{id: id})
        },
      AgentKind::Strategic => {
          return Agent::Strategic(StrategicAgent{id: id})
        },
    }
  }
}

pub struct Agents {
  data: HashMap<u64, Agent>
}

impl Agents {
  pub fn new() -> Agents {
    return Agents{data: HashMap::new()}
  }

  pub fn add(&mut self, _agent: Agent) {
    /*
    match &agent {
      &Agent::Joystick(ref a) => {
          let id = a.id;
          self.data.insert(id, agent);
        },
      &Agent::Reactive(_) => { unimplemented!(); },
      &Agent::BDI(_) => { unimplemented!(); },
      &Agent::Strategic(_) => { unimplemented!(); },
    };
    */
    unimplemented!();
  }

  pub fn remove(&mut self, id: u64) {
    self.data.remove(&id);
  }

  pub fn get(&mut self, id: u64) -> Option<&mut Agent> {
    let res = self.data.get_mut(&id);
    return res
  }
}

