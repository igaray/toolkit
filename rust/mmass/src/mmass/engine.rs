// Engine
use std::thread;
use std::time::Duration;
use std::sync;
use std::sync::mpsc::channel;

use mmass::repl as repl;
use mmass::config as config;
use mmass::scenario as scenario;
use mmass::global_env as global_env;
use mmass::local_env as local_env;

enum EngineState {
  Init,
  Final
}

/* Messages to the engine prefixed with 'Msg' do not expect a response.
 * Messagew prefixed with 'Req' are expected to be responded by the corresponding 'Res' message.
 */
#[derive(Debug)]
pub enum EngineMessage {
  MsgStart,
  MsgLoad,
  MsgRun,
  MsgPause,
  MsgStep,
  MsgStop
}

pub struct Engine {
  engine_mailbox: sync::mpsc::Receiver<EngineMessage>,
  repl_mailbox: sync::mpsc::Sender<repl::ReplMessage>,
  config: config::Config,
  scenario: scenario::ScenarioConfig,
  state: EngineState,
  global_env: global_env::GlobalEnv,
  local_envs: Vec<local_env::LocalEnv>
}

impl Engine {
  pub fn new(
      engine_mailbox: sync::mpsc::Receiver<EngineMessage>,
      repl_mailbox: sync::mpsc::Sender<repl::ReplMessage>,
      config: config::Config,
      scenario: scenario::ScenarioConfig
    ) -> thread::JoinHandle<u32> {
    let mut engine = Engine{
      engine_mailbox: engine_mailbox,
      repl_mailbox: repl_mailbox,
      config: config,
      scenario: scenario,
      state: EngineState::Init,
      global_env: global_env::new(global_env::GlobalEnvKind::SquareGrid),
      local_envs: Vec::new()
      };
    let handle = thread::spawn(move || { engine.run(); 0 });
    return handle
  }

  pub fn run(&mut self) {
    loop {
      match self.engine_mailbox.try_recv() {
        Ok(EngineMessage::MsgStop) => {
          println!("Engine received stop signal.");
          break;
        }
        Ok(msg) => {
          println!("Engine received: {:?}", &msg);
          match self.state {
            EngineState::Init => {
            }
            EngineState::Final => {
            }
          }
        },
        Err(sync::mpsc::TryRecvError::Empty) => {
        }
        Err(sync::mpsc::TryRecvError::Disconnected) => {
          println!("Engine receiver disconnected.");
        }
      }
      thread::sleep(Duration::from_millis(500));
    }
  }
}

