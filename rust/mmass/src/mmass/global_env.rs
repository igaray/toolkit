// Global Environment

#[derive(Debug)]
#[derive(Deserialize)]
pub enum GlobalEnvKind {
  SquareGrid,
  HexGrid,
  Graph,
  GeodesicGrid
}

struct GlobalMapSquareGrid {
}

struct GlobalMapHexGrid {
}

struct GlobalMapGraph {
}

struct GlobalMapGeodesicGrid {
}

struct GlobalMap {
  square_grid: Option<GlobalMapSquareGrid>,
  hex_grid: Option<GlobalMapSquareGrid>,
  graph: Option<GlobalMapGraph>,
  geodesic_grid: Option<GlobalMapGeodesicGrid>
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
      graph: Option::None,
      geodesic_grid: Option::None
    }
  }
}

