#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

/* node vertex / arc link edge */

/* TYPE DEFINITIONS */

typedef struct {
  uint32_t data;
} vertex_t;

typedef struct {
  uint32_t v, w;
} edge_t;

typedef struct {
  uint32_t e;
  uint32_t v;
  uint32_t **adj;
} graph_t;

typedef graph_t* graph_p;

/* GRAPH ADT API */

/* [x] */ graph_p graph_init(uint32_t v);
/* [x] */ graph_p graph_copy(graph_p g);
/* [ ] */ graph_p graph_random(uint32_t v, uint32_t e);

/* [x] */ void graph_insert_edge(graph_p g, edge_t e);
/* [x] */ void graph_remove_edge(graph_p g, edge_t e);
/* [x] */ uint32_t graph_edges(graph_p g, edge_t[]);

/* [x] */ void graph_destroy(graph_p g);
/* [x] */ void graph_print(graph_p g);

/* GRAPH ADT IMPLEMENTATION */

uint32_t** matrix_uint32_t(uint32_t n, uint32_t m, uint32_t val) {
  uint32_t** t = malloc(sizeof(uint32_t*) * n);
  for (uint32_t i = 0; i < n; i++) {
    t[i] = malloc(sizeof(uint32_t) * m);
  }
  for (uint32_t i = 0; i < n; i++) {
    for (uint32_t j = 0; j < m; j++) {
      t[i][j] = val;
    }
  }
  return t;
}

graph_p graph_init(uint32_t v) {
  graph_p g = malloc(sizeof(graph_t));
  g->v = v;
  g->e = 0;
  g->adj = matrix_uint32_t(v, v, 0);
  return g;
}

graph_p graph_copy(graph_p g) {
  graph_p ng = NULL;
  ng = graph_init(g->v);
  ng->e = g->e;
  for (uint32_t i = 0; i < g->v; i++) {
    for (uint32_t j = 0; j < g->v; j++) {
      ng->adj[i][j] = g->adj[i][j];
    }
  }
  return ng;
}

graph_p graph_random(uint32_t v, uint32_t e) {
  graph_p g = graph_init(v);
  return g;
  for (uint32_t i = 0; i < e; i++) {
    edge_t e;
    e.v = rand() % v;
    e.w = rand() % v;
    graph_insert_edge(g, e);
  }
}

void graph_destroy(graph_p g) {
  for (uint32_t i = 0; i < g->v; i++) {
    free(g->adj[i]);
  }
  free(g->adj);
  free(g);
}

void graph_insert_edge(graph_p g, edge_t e) {
  if (g->adj[e.v][e.w] == 0) {
    g->adj[e.v][e.w] = 1;
    g->adj[e.w][e.v] = 1;
  }
}

void graph_remove_edge(graph_p g, edge_t e) {
  if (g->adj[e.v][e.w] == 1) {
    g->adj[e.v][e.w] = 0;
    g->adj[e.w][e.v] = 0;
  }
}

uint32_t graph_edges(graph_p g, edge_t es[]) {
  uint32_t i = 0;
  edge_t e;
  for (uint32_t v = 0; v < g->v; v++) {
    for (uint32_t w = v; w < g->v; w++) {
      if (g->adj[v][w] == 1) {
        e.v = v;
        e.w = w;
        es[i++] = e;
      }
    }
  }
  return i;
}

void graph_print(graph_p g) {
  for (uint32_t i = 0; i < g->v; i++) {
    for (uint32_t j = 0; j < g->v; j++) {
      printf("%d ", g->adj[i][j]);
    }
    printf("\n");
  }
}

/* MAIN */

int main(int argc, char** argv) {
  graph_p g = graph_init(8);
  edge_t e;
  e.v = 1;
  e.w = 3;

  printf("inserting edge\n");
  graph_insert_edge(g, e);
  graph_print(g);

  /*
  edge_t edges[8 * 7 / 2];
  printf("edges %d\n", graph_edges(g, edges));
  */

  printf("removing edge\n");
  graph_remove_edge(g, e);
  graph_print(g);

  graph_destroy(g);
  return EXIT_SUCCESS;
}

