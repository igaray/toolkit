use std::collections::HashMap;
use rand;

// Agents

#[derive(Debug, Deserialize, Serialize)]
pub struct JoystickAgent {
  id: u64,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ReactiveAgent {
  id: u64,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct BDIAgent {
  id: u64,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct StrategicAgent {
  id: u64,
}

#[derive(Debug, Deserialize, Serialize)]
pub enum AgentKind {
  Joystick,
  Reactive,
  BDI,
  Strategic,
}

#[derive(Debug, Deserialize, Serialize)]
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

#[derive(Debug, Deserialize, Serialize)]
pub struct Agents {
  data: HashMap<u64, Agent>
}

impl Agents {
  pub fn new() -> Agents {
    return Agents{data: HashMap::new()}
  }
}

