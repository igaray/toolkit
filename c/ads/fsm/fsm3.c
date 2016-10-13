#include <stdio.h>

typedef enum { EVT_A, EVT_B, EVT_C } event_t;
#define NO_OF_EVENTS 3

typedef struct fsm_s fsm_t;

typedef void (*state_proc_t)(fsm_t *fsmp, event_t event);

struct fsm_s {
  state_proc_t state;
};

void st_a(fsm_t *fsmp, event_t event);
void st_b(fsm_t *fsmp, event_t event);
void st_c(fsm_t *fsmp, event_t event);

void st_a(fsm_t *fsmp, event_t event) {
  switch (event) {
  case EVT_A: {
    printf("ST_A x EVT_A -> ST_A\n");
    fsmp->state = st_a;
    break;
  }
  case EVT_B: {
    printf("ST_A x EVT_B -> ST_B\n");
    fsmp->state = st_b;
    break;
  }
  case EVT_C: {
    printf("ST_A x EVT_C -> ST_C\n");
    fsmp->state = st_c;
    break;
  }
  }
}

void st_b(fsm_t *fsmp, event_t event) {
  switch (event) {
  case EVT_A: {
    printf("ST_B x EVT_A -> ST_A\n");
    fsmp->state = st_a;
    break;
  }
  case EVT_B: {
    printf("ST_B x EVT_B -> ST_B\n");
    fsmp->state = st_b;
    break;
  }
  case EVT_C: {
    printf("ST_B x EVT_C -> ST_C\n");
    fsmp->state = st_c;
    break;
  }
  }
}

void st_c(fsm_t *fsmp, event_t event) {
  switch (event) {
  case EVT_A: {
    printf("ST_C x EVT_A -> ST_A\n");
    fsmp->state = st_a;
    break;
  }
  case EVT_B: {
    printf("ST_C x EVT_B -> ST_B\n");
    fsmp->state = st_b;
    break;
  }
  case EVT_C: {
    printf("ST_C x EVT_C -> ST_C\n");
    fsmp->state = st_c;
    break;
  }
  }
}

void fsm_init(fsm_t *fsmp) { fsmp->state = st_a; }

void fsm_transition(fsm_t *fsmp, event_t event) { fsmp->state(fsmp, event); }

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
