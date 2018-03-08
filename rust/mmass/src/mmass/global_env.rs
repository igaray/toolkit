// Global Environment

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum GlobalEnvKind {
  SquareGrid,
  HexGrid,
  GeodesicGrid,
  Graph
}

#[derive(Debug, Deserialize, Serialize)]
struct GlobalMapSquareGrid {
}

#[derive(Debug, Deserialize, Serialize)]
struct GlobalMapHexGrid {
}

#[derive(Debug, Deserialize, Serialize)]
struct GlobalMapGeodesicGrid {
}

#[derive(Debug, Deserialize, Serialize)]
struct GlobalMapGraph {
}

#[derive(Debug, Deserialize, Serialize)]
struct GlobalMap {
  square_grid: Option<GlobalMapSquareGrid>,
  hex_grid: Option<GlobalMapSquareGrid>,
  geodesic_grid: Option<GlobalMapGeodesicGrid>,
  graph: Option<GlobalMapGraph>
}

#[derive(Debug, Deserialize, Serialize)]
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

