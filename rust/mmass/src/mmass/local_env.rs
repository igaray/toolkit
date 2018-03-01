// Local Environment
use mmass::agent as agent;
use mmass::percept as percept;
use mmass::action as action;
use mmass::object as object;
use mmass::order as order;
use mmass::area as area;
use mmass::event as event;

#[derive(Debug)]
pub enum Material {
  Earth,
  Air,
  Water
}

#[derive(Debug)]
pub struct Cell {
  material: Material
}

#[derive(Debug)]
pub struct LocalMapSquareGrid {
}

#[derive(Debug)]
pub struct LocalMapHexGrid {
}

#[derive(Debug)]
pub struct LocalMapVoxelGrid {
}

#[derive(Debug)]
pub struct LocalMapGraph {
}

#[derive(Debug)]
pub struct LocalMapFree {
}

#[derive(Debug)]
pub enum LocalEnvMap {
  SquareGrid(LocalMapSquareGrid),
  HexGrid(LocalMapHexGrid),
  VoxelGrid(LocalMapVoxelGrid),
  Graph(LocalMapGraph),
  Free(LocalMapFree),
}

#[derive(Debug, Deserialize)]
pub enum LocalEnvMapKind {
  SquareGrid,
  HexGrid,
  VoxelGrid,
  Graph,
  Free,
}

pub struct LocalEnv {
  map: LocalEnvMap,
  agents: agent::Agents,
  percepts: percept::Percepts,
  actions: action::Actions,
  objects: object::Objects,
  orders: order::Orders,
  areas: area::Areas,
  events: event::Events,
}

pub fn new(map: LocalEnvMap) -> LocalEnv {
  let local_env = LocalEnv{
    map: map,
    agents: agent::Agents::new(),
    percepts: percept::Percepts::new(),
    actions: action::Actions::new(),
    objects: object::Objects::new(),
    orders: order::Orders::new(),
    areas: area::Areas::new(),
    events: event::Events::new(),
  };
  return local_env
}

