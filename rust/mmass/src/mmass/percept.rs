use std::collections::HashMap;

// Percepts
pub enum PerceptKind {
  Basic
}

pub struct Percept {
  id: u64,
  kind: PerceptKind,
  fov: Vec<TerrainDescription>,
  agents: Vec<AgentDescription>,
  objects: Vec<ObjectDescription>
}

pub struct TerrainDescription {
}

pub struct AgentDescription {
  id: u64
}

pub struct ObjectDescription {
  id: u64
}

pub struct Percepts {
  data: HashMap<u64, Percept>
}

impl Percepts {
  pub fn new() -> Percepts {
    return Percepts{data: HashMap::new()}
  }

  pub fn add(&mut self, percept: Percept) {
    self.data.insert(percept.id, percept);
  }

  pub fn remove(&mut self, id: u64) {
    self.data.remove(&id);
  }

  pub fn get(&mut self, id: u64) -> Option<&Percept> {
    let res = self.data.get(&id);
    return res
  }
}

