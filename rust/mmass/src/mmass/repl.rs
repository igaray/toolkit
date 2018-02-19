use std::io;
use std::io::Write;
use std::io::BufRead;
use std::collections::HashMap;

pub enum ReplCommandKind {
  Unknown,
  Start,
  WorldGen,
  Inspect
}

pub struct ReplCommand{
  kind: ReplCommandKind,
  params: HashMap<String, String>,
}

pub fn run() {
  let stdin = io::stdin();
  let mut stdout = io::stdout();
  loop {
    print!("> ");
    stdout.flush().expect("Error flushing stdout");
    let mut line = String::new();
    stdin.lock().read_line(&mut line).expect("Error reading from stdin");
    match line.as_str() {
      "quit\n" => {
        println!("quitting...");
        break;
      },
      _ => {
        let command = command(line);
        execute_command(command);
      }
    }
  }
}

fn command(input: String) -> ReplCommand {
  println!("echo: {:?}", input);
  let tokens = tokenize(input);
  let command = parse(tokens);
  return command
}

fn tokenize(input: String) -> Vec<String> {
  let tokens = Vec::new();
  return tokens
}

fn parse(tokens: Vec<String>) -> ReplCommand {
  let params = HashMap::new();
  let command = ReplCommand{kind: ReplCommandKind::Unknown, params: params};
  return command
}

fn execute_command(command: ReplCommand) {
}

