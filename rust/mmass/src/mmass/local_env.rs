// Local Environment
use mmass::agent as agent;
use mmass::action as action;

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Position {
  pub x: u8,
  pub y: u8,
}

#[derive(Debug, Deserialize, Serialize)]
pub enum Direction {
  North,
  East,
  South,
  West
}
#[derive(Debug, Deserialize, Serialize)]
pub enum Material {
  Earth,
  Water,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Cell {
  pub material: Material
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LocalEnvMap {
  pub cells: Vec<Vec<Cell>>,
}

impl LocalEnvMap {
  pub fn new(height: u8, width: u8) -> LocalEnvMap {
    let mut cells = Vec::new();
    for _i in 1..height {
      let mut row = Vec::new();
      for _j in 1..width {
        row.push(Cell{ material: Material::Earth });
      }
      cells.push(row);
    }
    return LocalEnvMap{ cells: cells }
  }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LocalEnv {
  pub name: String,
  pub map: LocalEnvMap,
  pub agents: agent::Agents,
  pub actions: action::Actions,
}

impl LocalEnv {
  pub fn generate(name: String, height: u8, width: u8) -> LocalEnv {
    let map = LocalEnvMap::new(height, width);
    let agents = agent::Agents::new();
    let local_env = LocalEnv{
      name: name,
      map: map,
      agents: agents,
      actions: action::Actions::new(),
    };
    return local_env
  }
}
