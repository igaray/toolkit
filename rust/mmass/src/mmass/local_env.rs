// Local Environment
use mmass::area as area;
use mmass::event as event;
use mmass::object as object;
use mmass::agent as agent;
use mmass::order as order;

pub enum LocalEnvKind {
  SquareGrid,
  HexGrid,
  Graph,
  Voxel,
  Free
}

struct LocalMapSquareGrid {
}

struct LocalMapHexGrid {
}

struct LocalMapGraph {
}

struct LocalMapVoxel {
}

struct LocalMapFree {
}

struct LocalEnvMap {
  square_grid: Option<LocalMapSquareGrid>,
  hex_grid: Option<LocalMapHexGrid>,
  graph: Option<LocalMapGraph>,
  voxel: Option<LocalMapVoxel>,
  free: Option<LocalMapFree>
}

pub struct LocalEnv {
  kind: LocalEnvKind,
  map: LocalEnvMap,
  areas: Vec<area::Area>,
  events: Vec<event::Event>,
  objects: Vec<object::Object>,
  agents: Vec<agent::Agent>,
  orders: Vec<order::Order>
}

pub fn new() -> LocalEnv {
  return LocalEnv{
    kind: LocalEnvKind::SquareGrid,
    map: LocalEnvMap{
      square_grid: Option::Some(LocalMapSquareGrid{}),
      hex_grid: Option::None,
      graph: Option::None,
      voxel: Option::None,
      free: Option::None
      },
    areas: Vec::new(),
    events: Vec::new(),
    objects: Vec::new(),
    agents: Vec::new(),
    orders: Vec::new()
  }
}

