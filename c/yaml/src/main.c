#include "yaml.h"
#include <stdio.h>

typedef enum return_values {
  OK,
  ERR_ARG_NUM,
  ERR_FILE_OPEN_FAIL,
  ERR_YAML_INIT_FAIL,
  ERR_DOC_LOAD_FAIL,
  ERR_PARSE_FAIL,
  ERR_UNEXPECTED_NODE_TYPE
} return_value_t;

void yaml_version() {
  int major = 0, minor = 0, patch = 0;
  yaml_get_version(&major, &minor, &patch);
  printf("YAML %d.%d.%d\n", major, minor, patch);
}

int print_document(yaml_document_t *document, yaml_node_t *node) {
  int rv = 0;
  switch (node->type) {
  case YAML_NO_NODE: {
    printf("node type: no node\n");
    break;
  }
  case YAML_SCALAR_NODE: {
    size_t size = node->data.scalar.length;
    unsigned char *value = node->data.scalar.value;
    printf("node type: scalar [length: %zu, value: %s] \n", size, value);
    break;
  }
  case YAML_SEQUENCE_NODE: {
    printf("node type: sequence\n");
    int n = node->data.sequence.items.top - node->data.sequence.items.start;
    for (int i = 0; i < n; i++) {
      int child_node_idx = node->data.sequence.items.start[i];
      yaml_node_t *child_node =
          yaml_document_get_node(document, child_node_idx);
      rv = print_document(document, child_node);
    }
    break;
  }
  case YAML_MAPPING_NODE: {
    printf("node type: mapping\n");
    int n = node->data.mapping.pairs.top - node->data.mapping.pairs.start;
    for (int i = 0; i < n; i++) {
      int child_node_k_idx = node->data.mapping.pairs.start[i].key;
      int child_node_v_idx = node->data.mapping.pairs.start[i].value;

      yaml_node_t *child_node_k =
          yaml_document_get_node(document, child_node_k_idx);
      yaml_node_t *child_node_v =
          yaml_document_get_node(document, child_node_v_idx);

      rv = print_document(document, child_node_k);
      if (rv) {
        break;
      }
      rv = print_document(document, child_node_v);
      if (rv) {
        break;
      }
    }
    break;
  }
  default: {
    printf("ERROR: unexpected node type: %d\n", node->type);
    rv = ERR_UNEXPECTED_NODE_TYPE;
  }
  }
  return rv;
}

int load_documents(char *config_file_path) {
  int rv = 0;
  FILE *config_file = NULL;
  yaml_parser_t parser;
  yaml_document_t document;

  config_file = fopen(config_file_path, "r");

  if (config_file == NULL) {
    printf("ERROR: failed to open file %s\n", config_file_path);
    return ERR_FILE_OPEN_FAIL;
  }
  if (!yaml_parser_initialize(&parser)) {
    printf("ERROR: failed to initialize YAML parser\n");
    return ERR_YAML_INIT_FAIL;
  }

  yaml_parser_set_input_file(&parser, config_file);

  int done = 0;
  while (!done) {
    if (!yaml_parser_load(&parser, &document)) {
      printf("ERROR: failed to load document\n");
      return ERR_DOC_LOAD_FAIL;
    }

    yaml_node_t *root_node = yaml_document_get_root_node(&document);
    if (root_node == NULL) {
      done = 1;
    } else {
      print_document(&document, root_node);
    }
    yaml_document_delete(&document);
  }

  yaml_parser_delete(&parser);
  fclose(config_file);
  return rv;
}

int main(int argc, char *argv[]) {
  int rv = OK;

  if (argc != 2) {
    printf("ERROR: incorrect number of arguments\n");
    return ERR_ARG_NUM;
  }

  // yaml_version();

  rv = load_documents(argv[1]);
  if (rv) {
    printf("ERROR: %d", rv);
  }
  return rv;
}
