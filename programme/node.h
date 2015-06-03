#ifndef NODE_H
#define NODE_H

#include "symbol_table.h"

typedef enum { PROGRAM, ASSIGN, IF, WHILE, STATEMENT, CONST, VAR, TYPE, EXPR, IDENTIFIER, OP, FOR, VAR_LIST, IDENT_LIST_TYPE, SIMPLE_TYPE_INT, SIMPLE_TYPE_REAL, SIMPLE_TYPE_BOOL, COMP_STMT, IO_READ, IO_WRITE, FOR_TO, FOR_DOWNTO, IDENTIFIER_SUBSCRIPT, FACTOR_NOT, FACTOR_MINUS, FACTOR_EXPR } node_type;
typedef enum { PLUS, MINUS, MUL, DIV, MOD, LT, LE, GT, GE, EQ, NE, AND, OR, SLASH } operator;

typedef struct _node {
  node_type type;
  union {
    operator op;
    entry *symbol;
    char *ident_temp;
    /* list of BNF right-hand side symbols of nonterminal type */
    struct _node **body;
  };
  struct _node *next; /* decl-list, stmt-list */
} node;

node *new_node(const node_type type);
int get_body_count(const node_type type);
void free_node(node *node);
void print_node(const node * const node);

#endif
