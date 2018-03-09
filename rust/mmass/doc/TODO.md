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
[ ] show repl command history
[ ] engine fsm
[ ] start new sim
[ ] basic perceive-act loop
[ ] pause sim
[ ] step through sim
[ ] save current sim
[ ] load sim
[ ] trivial world gen
[ ] ncurses or similar for repl, readline functionality

Future work
[ ] make joystick agent
[ ] percepts
[ ] percept field of view
[ ] orders
[ ] make agents get orders
[ ] pathfinding
[ ] goto order
[ ] objects
[ ] areas
[ ] 
[ ] collections
  [ ] agents
  [ ] actions
  [ ] percepts
  [ ] areas
  [ ] events
  [ ] objects
  [ ] orders
