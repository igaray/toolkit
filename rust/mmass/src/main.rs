// Engine Configuration
mod config {
  pub struct Config {
  }

  pub fn load() -> Config {
    let config = Config{};
    return config;
  }
}

// Scenario Configuration
mod scenario {
}

// Global Environment
mod global_env {

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

  pub fn new() -> GlobalEnv {
    return GlobalEnv{
      kind: GlobalEnvKind::SquareGrid,
      map: GlobalMap{
        square_grid: Option::Some(GlobalMapSquareGrid{}),
        hex_grid: Option::None,
        graph: Option::None,
        geodesic_grid: Option::None
      }
    }
  }
}

// Local Environment
mod local_env {
  use area;
  use event;
  use object;
  use agent;
  use order;

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
}

// Areas
mod area {
  pub struct Area {
  }
}

// Events
mod event {
  pub enum EventKind {
  }

  pub struct Event {
    kind: EventKind
  }
}

// Orders
mod order {
  pub enum OrderKind {
  }

  pub struct Order {
    kind: OrderKind
  }
}

// Agents
mod agent {
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
}

// Percepts
mod percept {
  pub enum PerceptKind {
  }

  pub struct Percept {
    kind: PerceptKind
  }
}

// Actions
mod action {
  pub enum ActionKind {
    Noop,
    Move,
    Reaction
  }

  pub struct Action {
    kind: ActionKind
  }
}

// Objects
mod object {
  pub struct Object {
    id: u64
  }
}

// Engine
mod simulation_engine {

  use global_env;
  use local_env;

  enum SimulationEngineState {
    Init,
    Final
  }

  pub struct SimulationEngine {
    state: SimulationEngineState,
    global_env: global_env::GlobalEnv,
    local_envs: Vec<local_env::LocalEnv>
  }

  pub fn new() -> SimulationEngine {
    return SimulationEngine{
      state: SimulationEngineState::Init,
      global_env: global_env::new(),
      local_envs: Vec::new()
      }
  }
}

fn main() {
  let engine = simulation_engine::new();
}
