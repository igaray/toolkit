use std::thread;
use std::sync;

use mmass::config as config;
use mmass::scenario as scenario;
use mmass::local_env as local_env;

#[derive(Debug)]
enum EngineState {
  Init,
  Running,
  Paused,
  Final
}

/* Messages to the engine prefixed with 'Msg' do not expect a response.
 * Messagew prefixed with 'Req' are expected to be responded by the corresponding 'Res' message.
 */
#[derive(Debug)]
pub enum EngineMessage {
  ReqGenerate{ name: String },
  ResGenerate{ env: local_env::LocalEnv },
  MsgStart{ world_name: String, scenario_name: String },
  MsgLoad{ savefile: String },
  MsgRun,
  MsgPause,
  MsgStep{ steps: u8 },
  MsgSave{ savefile: String },
  MsgStop,
  MsgQuit,
}

pub struct Engine {
  engine_mailbox: sync::mpsc::Receiver<EngineMessage>,
  repl_mailbox: sync::mpsc::Sender<EngineMessage>,
  config: config::Config,
  scenario: scenario::ScenarioConfig,
  state: EngineState,
  local_env: Option<local_env::LocalEnv>,
}

impl Engine {
  pub fn new(
      engine_mailbox: sync::mpsc::Receiver<EngineMessage>,
      repl_mailbox: sync::mpsc::Sender<EngineMessage>,
      config: config::Config,
      scenario: scenario::ScenarioConfig
    ) -> thread::JoinHandle<u32> {
    let mut engine = Engine{
      engine_mailbox: engine_mailbox,
      repl_mailbox: repl_mailbox,
      config: config,
      scenario: scenario,
      state: EngineState::Init,
      local_env: None,
      };
    let builder = thread::Builder::new().name("Engine".into());
    let handle = builder.spawn(move || { engine.run(); 0 }).unwrap();
    return handle
  }

  pub fn run(&mut self) {
    loop {
      match self.state {
        EngineState::Init => self.state_init(),
        EngineState::Running => self.state_running(),
        EngineState::Paused => self.state_paused(),
        EngineState::Final => {
          self.state_final();
          break;
        }
      }
    }
  }

  fn state_init(&mut self) {
    debug!("State: Init");
    match self.engine_mailbox.recv().unwrap() {
      EngineMessage::ReqGenerate{ ref name } => {
        debug!("Message: MsgGenerate");
        // TODO execute worldgen
        let env = local_env::LocalEnv::generate(name.clone(), 10, 10);
        let msg = EngineMessage::ResGenerate{ env };
        self.repl_mailbox.send(msg).unwrap();
      },
      EngineMessage::MsgStart{ world_name: ref _world_name, scenario_name: ref _scenario_name } => {
        debug!("Message: MsgStart");
        self.state = EngineState::Paused;
        unimplemented!();
      },
      EngineMessage::MsgLoad{ savefile: ref _savefile } => {
        debug!("Message: MsgLoad");
        self.state = EngineState::Paused;
        unimplemented!();
      },
      EngineMessage::MsgQuit => {
        debug!("Message: MsgQuit");
        self.state = EngineState::Final;
      },
      _msg => {
        error!("Unexpected message. state: {:?} msg: {:?}", self.state, _msg);
      }
    }
  }

  fn state_paused(&mut self) {
    debug!("State: Paused");
    match self.engine_mailbox.recv().unwrap() {
      EngineMessage::MsgRun => {
        debug!("Message: MsgRun.");
        self.state = EngineState::Running;
        unimplemented!();
      },
      EngineMessage::MsgStep{ steps: ref _steps } => {
        debug!("Message: MsgStep.");
        unimplemented!();
      },
      EngineMessage::MsgSave{ savefile: ref _savefile } => {
        debug!("Message: MsgSave.");
        unimplemented!();
      },
      EngineMessage::MsgStop => {
        debug!("Message: MsgStop.");
        self.state = EngineState::Init;
        unimplemented!();
      },
      EngineMessage::MsgQuit => {
        debug!("Message: MsgQuit.");
        self.state = EngineState::Final;
      },
      _msg => {
        error!("Unexpected message. state: {:?} msg: {:?}", self.state, _msg);
      }
    }
  }

  fn state_running(&mut self) {
    debug!("State: Running");
    match self.engine_mailbox.try_recv().unwrap() {
      EngineMessage::MsgPause => {
        debug!("Message: MsgPause.");
        self.state = EngineState::Paused;
        unimplemented!();
      },
      EngineMessage::MsgStop => {
        debug!("Message: MsgStop.");
        self.state = EngineState::Init;
        unimplemented!();
      },
      EngineMessage::MsgQuit => {
        debug!("Message: MsgQuit.");
        self.state = EngineState::Final;
      },
      _msg => {
        error!("Unexpected message. state: {:?} msg: {:?}", self.state, _msg);
      }
    }
  }

  fn state_final(&mut self) {
    debug!("State: Final");
  }
}

