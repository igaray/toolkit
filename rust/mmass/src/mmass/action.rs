// Actions
mod action {
  pub enum ActionKind {
    Noop,
    Move,
    Reaction
  }

  pub struct Action {
    id: u64,
    kind: ActionKind
  }

  pub struct Actions {
    _data: Vec<Action>
  }

  impl Actions {
    fn add(action: Action) -> Result<(), ()> {
      let res = Ok(());
      return res
    }

    fn remove(id: u64) -> Result<(), ()> {
      let res = Ok(());
      return res
    }

    fn find(id: u64) -> Result<Action, ()> {
      let action = Action{id: 0, kind: ActionKind::Noop};
      let res = Ok(action);
      return res
    }
  }
}

