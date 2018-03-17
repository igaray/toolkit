# TODO

[x] add logging
[x] random id generation
[x] repl
[x] config & loading
[x] setting global and local env kind through scenario
[x] move config and scenario to yaml
[x] load agents from scenario config
[ ] make agent loading not require the id in the scenario config
    if the id is not present, create a new random one
[x] engine and repl run in separate threads
[x] send commands from repl to engine
[x] create channels in main, outside of the the functions, so that both repl and 
    engine can have each other's senders.
[x] remove globalenv and simplify localenv
[x] engine fsm
[x] trivial square grid world gen
[x] save generated world
[x] list existing worlds
[ ] deserialize a saved world
[x] merge global and scenario config, maybe one file, maybe two, but only one config object
[ ] start new sim in a generated world and scenario
[ ] basic perceive-act loop with empty percepts and noop actions for all agents
[ ] pause sim
[ ] step through sim
[ ] make savefile name optional in save command, if none, save under default name
[ ] save current sim
[x] list existing savefiles
[ ] deserialize a saved sim
[ ] load sim
[ ] make joystick agent

Future work
[ ] write documentation in help command for all other commands
[ ] list scenarios, worlds and savefiles in alphabetical order
[ ] percepts
[ ] percept field of view
[ ] orders
[ ] - noop
[ ] - move
[ ] - grab
[ ] - drop
[ ] make agents get orders
[ ] pathfinding
[ ] goto order
[ ] objects
[ ] areas
[ ] collections
  [ ] agents
  [ ] actions
  [ ] percepts
  [ ] areas
  [ ] events
  [ ] objects
  [ ] orders
[ ] polish
[ ] tests
[ ] coverage
[ ] compress world and save files
[ ] show repl command history
[ ] ncurses or similar for repl, readline functionality
[ ] engine process socket/zeromq/nanomsg + protobuf/capnproto
[ ] separate repl and engine
[ ] ui process
