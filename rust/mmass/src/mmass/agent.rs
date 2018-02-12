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
  pub fn new() -> Agent {
    return Agent{id: 0, kind: AgentKind::Joystick}
  }
}

pub struct Agents {
  _data: Vec<Agent>
}

impl Agents {
  pub fn new() -> Agents {
    return Agents{_data: Vec::new()}
  }

  pub fn add(agent: Agent) -> Result<(), ()> {
    return Ok(())
  }

  pub fn remove(id: u64) -> Result<(), ()> {
    return Ok(())
  }

  pub fn find(id: u64) -> Result<Agent, ()> {
    let agent = Agent{id: 0, kind: AgentKind::Joystick};
    return Ok(agent)
  }
}
