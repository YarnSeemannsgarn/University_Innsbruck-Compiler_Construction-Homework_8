#include "node.h"

#include <stdlib.h>
#include <stdio.h>
#include "parser.tab.h"

node *new_node(const node_type type){
  node *new_node = (node *)malloc(sizeof(node));
  new_node->type = type;

  int body_count = get_body_count(type);
  new_node->body = calloc(body_count, sizeof(node*));

  return new_node;
}

int get_body_count(const node_type type) {
  switch (type) {
  case PROGRAM:
  case WHILE:
  case IDENT_LIST_TYPE:
  case IDENTIFIER_SUBSCRIPT:
    return 2;
  case FOR:
    return 5;
  case ASSIGN:
  case IF:
  case EXPR:
  case TYPE:
    return 3;
  case STATEMENT:
  case VAR_LIST:
  case COMP_STMT:
  case IO_READ:
  case IO_WRITE:
  case FACTOR_NOT:
  case FACTOR_MINUS:
  case FACTOR_EXPR:
    return 1;
  default:
    return 0;
	}
}

void free_node(node *node) {
  if (node != NULL) {
    const int body_count = get_body_count(node->type);
    
    int i;
    for (i=0; i < body_count; i++) {
      free_node(node->body[i]);
    }
    if (body_count > 0) {
      free(node->body);
    }

    free_node(node->next);
    free(node);
  }
}

void print_node(const node * const node) {
	if (node == NULL) {
		return;
	}

	switch (node->type) {
	case PROGRAM:
		printf("program parsed_prog;\n");
		if (node->body[0] != NULL) {
			printf("var ");
			print_node(node->body[0]);
		}
		print_node(node->body[1]);
		printf(".\n\n");
		break;

	case VAR_LIST:
		print_node(node->body[0]);
		printf(";\n");
		print_node(node->next);
		break;

	case IDENT_LIST_TYPE:
		print_node(node->body[0]);
		printf(" : ");
		print_node(node->body[1]);
		break;

	case IDENTIFIER:
		printf("%s", node->symbol->symbol.identifier);
		if (node->next != NULL) {
			printf(", ");
			print_node(node->next);
		}
		break;

	case TYPE:
		if (node->body[0] != NULL) {
			printf("array [");
			print_node(node->body[0]);
			printf("..");
			print_node(node->body[1]);
			printf("] of ");
		}
		print_node(node->body[2]);
		break;

	case SIMPLE_TYPE_INT:
		printf("integer");
		break;

	case SIMPLE_TYPE_REAL:
		printf("real");
		break;

	case SIMPLE_TYPE_BOOL:
		printf("boolean");
		break;

	case COMP_STMT:
		printf("begin\n");
		print_node(node->body[0]);
		printf("\nend");
		break;

	case STATEMENT:
		print_node(node->body[0]);
		if (node->next != NULL) {
			printf(";\n");
			print_node(node->next);
		}
		break;

	case IO_READ:
		printf("read(");
		print_node(node->body[0]);
		printf(")");
		break;

	case IO_WRITE:
		printf("write(");
		print_node(node->body[0]);
		printf(")");
		break;

	case ASSIGN:
		print_node(node->body[0]);
		if (node->body[1] != NULL) {
			printf("[");
			print_node(node->body[1]);
			printf("]");
		}
		printf(" := ");
		print_node(node->body[2]);
		break;

	case IF:
		printf("if ");
		print_node(node->body[0]);
		printf(" then\n");
		print_node(node->body[1]);
		if (node->body[2] != NULL) {
			printf("\nelse\n");
			print_node(node->body[2]);
		}
		break;

	case WHILE:
		printf("while ");
		print_node(node->body[0]);
		printf(" do\n");
		print_node(node->body[1]);
		break;

	case FOR:
		printf("for ");
		print_node(node->body[0]);
		printf(" := ");
		print_node(node->body[1]);
		print_node(node->body[2]);
		print_node(node->body[3]);
		printf(" do\n");
		print_node(node->body[4]);
		break;

	case FOR_TO:
		printf(" to ");
		break;

	case FOR_DOWNTO:
		printf(" downto ");
		break;

	case EXPR:
		print_node(node->body[0]);
		print_node(node->body[1]);
		print_node(node->body[2]);
		if (node->next != NULL) {
			printf(", ");
			print_node(node->next);
		}
		break;

	case CONST:
		switch (node->symbol->dtype) {
		case _BOOL:
			if (node->symbol->symbol.int_val) {
				printf("true");
			} else {
				printf("false");
			}
			break;

		case _STRING:
			printf("%s", node->symbol->symbol.identifier);
			break;

		case _INT:
			printf("%d", node->symbol->symbol.int_val);
			break;

		case _REAL:
			printf("%f", node->symbol->symbol.real_val);
			break;
		}
		break;

	case IDENTIFIER_SUBSCRIPT:
		print_node(node->body[0]);
		printf("[");
		print_node(node->body[1]);
		printf("]");
		break;

	case FACTOR_NOT:
		printf("not ");
		print_node(node->body[0]);
		break;

	case FACTOR_MINUS:
		printf("- ");
		print_node(node->body[0]);
		break;

	case FACTOR_EXPR:
		printf("(");
		print_node(node->body[0]);
		printf(")");
		break;

	case OP:
		switch (node->op) {
		case LT: printf(" < "); break;
		case LE: printf(" <= "); break;
		case GT: printf(" > "); break;
		case GE: printf(" >= "); break;
		case EQ: printf(" = "); break;
		case NE: printf(" <> "); break;
		case PLUS: printf(" + "); break;
		case MINUS: printf(" - "); break;
		case OR: printf(" or "); break;
		case MUL: printf(" * "); break;
		case SLASH: printf(" / "); break;
		case DIV: printf(" div "); break;
		case MOD: printf(" mod "); break;
		case AND: printf(" and "); break;
		}
		break;
	}
}
