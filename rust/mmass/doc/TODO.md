# TODO

[x] add logging
[x] random id generation
[x] repl
[x] config & loading
[x] setting global and local env kind through scenario
[x] move config and scenario to yaml
[x] load agents from scenario config
[x] engine and repl run in separate threads
[x] send commands from repl to engine
[x] create channels in main, outside of the the functions, so that both repl and 
    engine can have each other's senders.
[x] remove globalenv and simplify localenv
[x] engine fsm
[x] trivial square grid world gen
[x] save generated world
[x] list existing worlds
[x] deserialize a saved world
[x] merge global and scenario config, maybe one file, maybe two, but only one config object
[x] start new sim in a generated world and scenario
[x] basic perceive-act loop with empty percepts and noop actions for all agents
[x] pause sim
[x] step through sim
[ ] list agents
[ ] inspect agent
[ ] starting location selection for agents
[ ] write documentation in help command for all other commands
[ ] list scenarios, worlds and savefiles in alphabetical order
[ ] save current sim
[ ] make savefile name optional in save command, if none, save under default name
[x] list existing savefiles
[ ] deserialize a saved sim
[ ] load sim
[ ] make joystick agent
[ ] percepts
  [ ] percept agents
  [ ] percept field of view
[ ] entity component system
  [ ] embodied agents
  [ ] action pre and post condition checking and applying
[ ] objects
  [ ] agent inventory
  [ ] action: pickup
  [ ] action: drop
  [ ] percept objects
[ ] action conflict resolution
[ ] agent movement
  [ ] plan (list of pre-planned actions)
  [ ] pathfinding
[ ] orders
  [ ] - noop
  [ ] - move
  [ ] - goto
  [ ] - grab
  [ ] - drop
  [ ] list orders
  [ ] inspect order
  [ ] make agents get orders

Future work
[ ] fix your time step
[ ] gui: tcod
[ ] areas
[ ] events
[ ] effects
[ ] collections
[ ] polish
[ ] tests
[ ] coverage
[ ] compress world and save files
[x] make agent loading not require the id in the scenario config
    if the id is not present, create a new random one
[ ] unbuffered repl input
[ ] show repl command history
[ ] repl keybindings
[ ] ncurses or similar for repl, readline functionality
[ ] engine process socket/zeromq/nanomsg + protobuf/capnproto
[ ] separate repl/ui and engine
[ ] more types of local environments
[ ] global
  [ ] global engine fsm states
  [ ] global environments
  [ ] global agents
  [ ] global objects
