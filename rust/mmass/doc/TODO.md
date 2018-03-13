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
[ ] save generated world
[ ] list existing worlds
[ ] start new sim in a generated world and scenario
[ ] basic perceive-act loop with empty percepts and noop actions for all agents
[ ] pause sim
[ ] step through sim
[ ] save current sim
[ ] list existing savefiles
[ ] load sim
[ ] write documentation in help command for all other commands
[ ] make joystick agent

Future work
[ ] show repl command history
[ ] percepts
[ ] percept field of view
[ ] orders
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
[ ] ncurses or similar for repl, readline functionality
