// Agents
pub enum AgentKind {
  Reactive,
  StateBased,
  BDI,
  Strategic
}

pub struct Agent {
  kind: AgentKind,
  id: u64
}

