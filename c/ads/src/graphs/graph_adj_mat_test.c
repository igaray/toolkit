/*
 * Directed graph, with positive non-zero labels on arcs implemented with an
 * adjacency matrix.
 */

#include <stdio.h>
#include <stdlib.h>
#define MAX_NODES 10

typedef unsigned int uint;

typedef struct {
  uint max_node;
  uint nodes[MAX_NODES];
  uint adj[MAX_NODES][MAX_NODES];
} graph_t;

void graph_init(graph_t *g) {
  uint i, j;
  g->max_node = 0;
  for (i = 0; i < MAX_NODES; i++) {
    g->nodes[i] = 0;
  }
  for (i = 0; i < MAX_NODES; i++) {
    for (j = 0; j < MAX_NODES; j++) {
      g->adj[i][j] = 0;
    }
  }
}

void graph_destroy(graph_t *g) { free(g); }

void graph_print(graph_t *g) {
  uint i, j;
  printf("Graph:\n");
  printf("  Max Node Index: %d\n", g->max_node);
  printf("  Nodes: ");
  for (i = 0; i < g->max_node; i++) {
    if (g->nodes[i] != 0) {
      printf("%d ", i);
    }
  }
  printf("\n  Arcs:\n");
  for (i = 0; i < g->max_node; i++) {
    printf("    ");
    for (j = 0; j < g->max_node; j++) {
      printf("%d ", g->adj[i][j]);
    }
    printf("\n");
  }
}

void graph_add_node(graph_t *g, uint node) {
  if (node < MAX_NODES) {
    g->nodes[node] = 1;
    if (node >= g->max_node) {
      g->max_node = node + 1;
    }
  }
}

void graph_del_node(graph_t *g, uint node) {
  uint i, j;
  if (node < MAX_NODES) {
    g->nodes[node] = 0;
    for (i = 0; i < g->max_node; i++) {
      for (j = 0; j < g->max_node; j++) {
        if ((i == node) || (j == node)) {
          g->adj[i][j] = 0;
        }
      }
    }
    if (node == (g->max_node - 1)) {
      while ((g->nodes[g->max_node - 1] == 0) && (g->max_node > 0)) {
        g->max_node -= 1;
      }
    }
  }
}

void graph_add_arc(graph_t *g, uint node1, uint node2, uint weight) {
  if ((node1 < MAX_NODES) && (node2 < MAX_NODES)) {
    g->adj[node1][node2] = weight;
  }
}

void graph_del_arc(graph_t *g, uint node1, uint node2) {
  if ((node1 < MAX_NODES) && (node2 < MAX_NODES)) {
    g->adj[node1][node2] = 0;
  }
}

int main() {
  graph_t *g = malloc(sizeof(graph_t));
  graph_init(g);
  graph_add_node(g, 1);
  graph_add_node(g, 2);
  graph_add_node(g, 3);
  graph_add_arc(g, 1, 2, 1);
  graph_add_arc(g, 1, 3, 1);
  graph_add_arc(g, 2, 1, 1);
  graph_print(g);
  graph_del_arc(g, 1, 2);
  graph_del_node(g, 2);
  graph_del_node(g, 3);
  graph_print(g);
  graph_destroy(g);
  return 0;
}
