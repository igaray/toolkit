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
pub struct LocalMapSquareGrid {
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LocalMapHexGrid {
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LocalMapVoxelGrid {
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LocalMapGraph {
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LocalMapFree {
}

#[derive(Debug, Deserialize, Serialize)]
pub enum LocalEnvMap {
  SquareGrid(LocalMapSquareGrid),
  HexGrid(LocalMapHexGrid),
  VoxelGrid(LocalMapVoxelGrid),
  Graph(LocalMapGraph),
  Free(LocalMapFree),
}

#[derive(Debug, Deserialize, Serialize)]
pub enum LocalEnvMapKind {
  SquareGrid,
  HexGrid,
  VoxelGrid,
  Graph,
  Free,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct LocalEnv {
  map: LocalEnvMap,
  agents: agent::Agents,
  percepts: percept::Percepts,
  actions: action::Actions,
}

pub fn new(map: LocalEnvMap) -> LocalEnv {
  let local_env = LocalEnv{
    map: map,
    agents: agent::Agents::new(),
    percepts: percept::Percepts::new(),
    actions: action::Actions::new(),
  };
  return local_env
}

