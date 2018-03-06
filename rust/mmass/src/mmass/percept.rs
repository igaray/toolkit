use std::collections::HashMap;

#[derive(Debug, Deserialize, Serialize)]
pub struct Percept {
  id: u64,
  fov: Vec<TerrainDescription>,
  agents: Vec<AgentDescription>,
  objects: Vec<ObjectDescription>
}

#[derive(Debug, Deserialize, Serialize)]
pub struct TerrainDescription {
}

#[derive(Debug, Deserialize, Serialize)]
pub struct AgentDescription {
  id: u64
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ObjectDescription {
  id: u64
}

#[derive(Debug, Deserialize, Serialize)]
pub struct Percepts {
  data: HashMap<u64, Percept>
}

impl Percepts {
  pub fn new() -> Percepts {
    return Percepts{data: HashMap::new()}
  }
}

