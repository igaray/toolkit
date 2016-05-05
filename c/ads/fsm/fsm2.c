#include <stdio.h>

typedef enum { ST_A, ST_B, ST_C, NO_OF_STATES } state_t;

typedef enum { EVT_A, EVT_B, EVT_C, NO_OF_EVENTS } event_t;

char *state_str(state_t state) {
  switch (state) {
  case ST_A: {
    return "ST_A";
  }
  case ST_B: {
    return "ST_B";
  }
  case ST_C: {
    return "ST_C";
  }
  default: { return "unknown_state"; }
  }
}

char *event_str(event_t event) {
  switch (event) {
  case EVT_A: {
    return "EVT_A";
  }
  case EVT_B: {
    return "EVT_B";
  }
  case EVT_C: {
    return "EVT_C";
  }
  default: { return "unknown_event"; }
  }
}

static state_t transition_table[NO_OF_STATES][NO_OF_EVENTS] = {
    {ST_A, ST_B, ST_C}, {ST_A, ST_B, ST_C}, {ST_A, ST_B, ST_C}};

struct fsm_s {
  state_t state;
};

typedef struct fsm_s fsm_t;

void fsm_init(fsm_t *fsmp) { fsmp->state = ST_A; }

int fsm_transition(fsm_t *fsmp, event_t event) {
  int err = 0;
  printf("%s x %s -> %s\n", state_str(fsmp->state), event_str(event),
         state_str(transition_table[fsmp->state][event]));
  fsmp->state = transition_table[fsmp->state][event];
  return err;
}

int main() {
  event_t events[] = {EVT_A, EVT_B, EVT_B, EVT_C, EVT_C,
                      EVT_A, EVT_C, EVT_B, EVT_A};
  fsm_t fsm;
  fsm_init(&fsm);
  size_t events_max = sizeof(events) / sizeof(event_t);
  printf("|E| = %zu\n", events_max);
  for (size_t i = 0; i < events_max; i++) {
    fsm_transition(&fsm, events[i]);
  }
}
