use std::thread;
use std::sync;

use mmass::config as config;
use mmass::scenario as scenario;
use mmass::local_env as local_env;

#[derive(Debug)]
enum EngineState {
  Init,
  Generating,
  // Running,
  // Paused,
  Final
}

/* Messages to the engine prefixed with 'Msg' do not expect a response.
 * Messagew prefixed with 'Req' are expected to be responded by the corresponding 'Res' message.
 */
#[derive(Debug)]
pub enum EngineMessage {
  ReqGenerate{ name: String },
  ResGenerate,
  MsgAccept,
  MsgReject,
  // MsgStart,
  // MsgLoad,
  MsgRun,
  MsgPause,
  // MsgStep,
  MsgStop,
  MsgQuit
}

pub struct Engine {
  engine_mailbox: sync::mpsc::Receiver<EngineMessage>,
  repl_mailbox: sync::mpsc::Sender<EngineMessage>,
  config: config::Config,
  scenario: scenario::ScenarioConfig,
  state: EngineState,
  local_env: local_env::LocalEnv
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
      local_env: local_env::LocalEnv::new(),
      };
    let builder = thread::Builder::new().name("Engine".into());
    let handle = builder.spawn(move || { engine.run(); 0 }).unwrap();
    return handle
  }

  pub fn run(&mut self) {
    loop {
      match self.state {
        EngineState::Init => {
          debug!("State: Init");
          match self.engine_mailbox.recv().unwrap() {
            EngineMessage::MsgQuit => {
              debug!("Message: MsgQuit");
              self.state = EngineState::Final;
            },
            EngineMessage::ReqGenerate{ name: ref _name } => {
              debug!("Message: MsgGenerate");
              self.state = EngineState::Generating;
              // TODO execute worldgen
              self.repl_mailbox.send(EngineMessage::ResGenerate).unwrap();
            },
            _msg => {
              error!("Unexpected message. state: {:?} msg: {:?}", self.state, _msg);
            }
          }
        },
        EngineState::Generating => {
          debug!("State: Generating");
          match self.engine_mailbox.recv().unwrap() {
            EngineMessage::MsgQuit => {
              debug!("Message: MsgQuit.");
              self.state = EngineState::Final;
            },
            EngineMessage::MsgAccept => {
              debug!("Message: MsgAccept. Saving generated world...");
              // TODO save generated world  
              self.state = EngineState::Init;
            },
            EngineMessage::MsgReject => {
              debug!("Message: MsgReject. Discarding generated world...");
              // TODO discard generated world
              self.state = EngineState::Init;
            },
            _msg => {
              error!("Unexpected message. state: {:?} msg: {:?}", self.state, _msg);
            }
          }
        },
        EngineState::Final => {
          debug!("State: Final");
          break;
        }
      }
    }
        /*
        Err(sync::mpsc::TryRecvError::Empty) => {
          thread::sleep(Duration::from_millis(500));
        }
        Err(sync::mpsc::TryRecvError::Disconnected) => {
          println!("Engine receiver disconnected.");
          break;
        }
        */
  }
}

