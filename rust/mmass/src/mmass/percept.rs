use rand;

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
pub struct Percept {
  id: u64,
  fov: Vec<TerrainDescription>,
  agents: Vec<AgentDescription>,
  objects: Vec<ObjectDescription>
}

impl Percept {
  pub fn new() -> Percept {
    let id = rand::random::<u64>();
    let percept = Percept{
      id: id,
      fov: Vec::new(),
      agents: Vec::new(),
      objects: Vec::new()
    };
    return percept
  }
}
