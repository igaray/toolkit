// Local Environment
use mmass::agent as agent;
use mmass::percept as percept;
use mmass::action as action;

#[derive(Debug, Deserialize, Serialize)]
pub enum Material {
  Earth,
  Air,
  Water
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Cell {
  material: Material
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LocalEnvMap {
  cells: Vec<Cell>
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LocalEnv {
  map: LocalEnvMap,
  agents: agent::Agents,
  percepts: percept::Percepts,
  actions: action::Actions,
}

impl LocalEnv {
  pub fn new() -> LocalEnv {
    let local_env = LocalEnv{
      map: LocalEnvMap{ cells: Vec::new() },
      agents: agent::Agents::new(),
      percepts: percept::Percepts::new(),
      actions: action::Actions::new(),
    };
    return local_env
  }
}
