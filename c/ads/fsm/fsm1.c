#include <stdio.h>

typedef enum { ST_A, ST_B, ST_C } state_t;

typedef enum { EVT_A, EVT_B, EVT_C } event_t;

struct fsm_s {
  state_t state;
};

typedef struct fsm_s fsm_t;

void fsm_init(fsm_t *fsmp) { fsmp->state = ST_A; }

int fsm_transition_ST_A(fsm_t *fsmp, event_t event) {
  int err = 0;
  switch (event) {
  case EVT_A: {
    printf("ST_A x EVT_A -> ST_A\n");
    fsmp->state = ST_A;
    break;
  }
  case EVT_B: {
    printf("ST_A x EVT_B -> ST_B\n");
    fsmp->state = ST_B;
    break;
  }
  case EVT_C: {
    printf("ST_A x EVT_C -> ST_C\n");
    fsmp->state = ST_C;
    break;
  }
  default: { err = 2; }
  }
  return err;
}

int fsm_transition_ST_B(fsm_t *fsmp, event_t event) {
  int err = 0;
  switch (event) {
  case EVT_A: {
    printf("ST_B x EVT_A -> ST_A\n");
    fsmp->state = ST_A;
    break;
  }
  case EVT_B: {
    printf("ST_B x EVT_B -> ST_B\n");
    fsmp->state = ST_B;
    break;
  }
  case EVT_C: {
    printf("ST_B x EVT_C -> ST_C\n");
    fsmp->state = ST_C;
    break;
  }
  default: { err = 3; }
  }
  return err;
}

int fsm_transition_ST_C(fsm_t *fsmp, event_t event) {
  int err = 0;
  switch (event) {
  case EVT_A: {
    printf("ST_C x EVT_A -> ST_A\n");
    fsmp->state = ST_A;
    break;
  }
  case EVT_B: {
    printf("ST_C x EVT_B -> ST_B\n");
    fsmp->state = ST_B;
    break;
  }
  case EVT_C: {
    printf("ST_C x EVT_C -> ST_C\n");
    fsmp->state = ST_C;
    break;
  }
  default: { err = 4; }
  }
  return err;
}

int fsm_transition(fsm_t *fsmp, event_t event) {
  int err = 0;
  switch (fsmp->state) {
  case ST_A: {
    err = fsm_transition_ST_A(fsmp, event);
    break;
  }
  case ST_B: {
    err = fsm_transition_ST_B(fsmp, event);
    break;
  }
  case ST_C: {
    err = fsm_transition_ST_C(fsmp, event);
    break;
  }
  default: { err = 1; }
  }
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
