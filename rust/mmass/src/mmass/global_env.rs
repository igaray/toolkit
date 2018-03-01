// Global Environment

#[derive(Debug)]
#[derive(Deserialize)]
pub enum GlobalEnvKind {
  SquareGrid,
  HexGrid,
  GeodesicGrid,
  Graph
}

struct GlobalMapSquareGrid {
}

struct GlobalMapHexGrid {
}

struct GlobalMapGeodesicGrid {
}

struct GlobalMapGraph {
}

struct GlobalMap {
  square_grid: Option<GlobalMapSquareGrid>,
  hex_grid: Option<GlobalMapSquareGrid>,
  geodesic_grid: Option<GlobalMapGeodesicGrid>,
  graph: Option<GlobalMapGraph>
}

pub struct GlobalEnv {
  kind: GlobalEnvKind,
  map: GlobalMap
}

pub fn new(kind: GlobalEnvKind) -> GlobalEnv {
  return GlobalEnv{
    kind: kind,
    map: GlobalMap{
      square_grid: Option::Some(GlobalMapSquareGrid{}),
      hex_grid: Option::None,
      geodesic_grid: Option::None,
      graph: Option::None
    }
  }
}

