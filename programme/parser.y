%{ 
#include <stdio.h>
#include <string.h>
#include "node.h"
#include "symbol_table.h"

unsigned int parser_err_ctr = 0;
node *root_node;

typedef enum { EXPR_PART_EXPR=0, EXPR_PART_SIMPLE_EXPR, EXPR_PART_TERM, EXPR_PART_FACTOR } expr_part;

entry *get_and_verify_ident_symbol(const char * const ident); // handles task a)
void declare_vars(node *identListType); // handles task b)
void check_array(const char *const ident, node *index_expr); // handles task c);
void check_array_decl(node *number1, node *number2); // handles task d);
void check_assign(const char *const ident, node *expr); // handles task e)
void check_illegal_op(node *left_node, node *op, node *right_node, expr_part expr_part1, expr_part expr_part2); // handles task f)
void check_condition(node *expr); // handles task g)

void yyerror(char *);
void combine_err_msg_1_repl(char const *part1, char const* part2);
void combine_err_msg_2_repl(char const *part1, char const* part2, char const* part3);
void combine_err_msg_3_repl(char const *part1, char const* part2, char const* part3, char const* part4);
void combine_err_msg_int(char const *part1, int part2);

// For task c), e), f) and g)
data_type get_expr_data_type(node *expr, expr_part part);
%}

%union {
    int integer;
    double real;
    char *string;
    struct _node *node;
}

// Tokens from homework 4
%token		/* Keywords */ T_PROGRAM T_VAR T_INTEGER T_ARRAY T_OF T_REAL T_BOOLEAN T_BEGIN T_WHILE T_DO T_IF T_THEN T_ELSE T_END T_FOR T_TO T_DOWNTO T_READ T_WRITE T_DIV T_MOD T_AND T_OR T_TRUE T_FALSE T_NOT 
                /* Special symbols */ T_SEMICOLON T_COMMA T_ASSIGNMENT T_COLON T_LEFT_SQUARE_BRACKET T_RIGHT_SQUARE_BRACKET T_DOT_DOT T_DOT T_LEFT_BRACKET T_RIGHT_BRACKET T_STAR T_SLASH T_PLUS T_MINUS T_UNEQUAL T_LESS_THAN T_GREATER_THAN T_GREATER_EQUAL_THAN T_LESS_EQUAL_THAN T_EQUAL

%token <string> T_ID T_STRING
%token <real> T_NUM_REAL
%token <integer> T_NUM_INT
			
%start	        start

// see http://www.gnu.org/software/bison/manual/html_node/Shift_002fReduce.html
// see http://www.gnu.org/software/bison/manual/html_node/Non-Operators.html#Non-Operators
%right T_THEN T_ELSE

%type <node> start varDec varDecList identListType identList type simpleType compStmt stmtList statement assignStmt ifStmt whileStmt forStmt toPart expr exprList simpleExpr term factor number relOp addOp mulOp
%%

// Grammar from homework 4
start                   : T_PROGRAM T_ID T_SEMICOLON varDec compStmt T_DOT { $$ = new_node(PROGRAM); root_node = $$; $$->body[0] = $4; $$->body[1] = $5; free($2); }
                        ;

varDec 		        : T_VAR varDecList { $$ = $2; }
		        | /* ε */ { $$ = NULL; }
                        ;

varDecList              : identListType T_SEMICOLON varDecList { $$ = new_node(VAR_LIST); $$->body[0] = $1; $$->next = $3; }
                        | identListType T_SEMICOLON { $$ = new_node(VAR_LIST); $$->body[0] = $1; }
                        ;

identListType           : identList T_COLON type { $$ = new_node(IDENT_LIST_TYPE); $$->body[0] = $1; $$->body[1] = $3; declare_vars($$); }
                        ;
 
identList               : T_ID T_COMMA identList { $$ = new_node(IDENTIFIER); $$->ident_temp = $1; $$->next = $3; }
                        | T_ID { $$ = new_node(IDENTIFIER); $$->ident_temp = $1; }
                        ;
		 
type                    : simpleType { $$ = new_node(TYPE); $$->body[2] = $1; }
| T_ARRAY T_LEFT_SQUARE_BRACKET number T_DOT_DOT number T_RIGHT_SQUARE_BRACKET T_OF simpleType { $$ = new_node(TYPE); $$->body[0] = $3; $$->body[1] = $5; $$->body[2] = $8; check_array_decl($3, $5); }
                        ;
 
simpleType	        : T_INTEGER { $$ = new_node(SIMPLE_TYPE_INT);  }
                        | T_REAL { $$ = new_node(SIMPLE_TYPE_REAL); }
                        | T_BOOLEAN { $$ = new_node(SIMPLE_TYPE_BOOL); }
                        ;
 
compStmt	        : T_BEGIN stmtList T_END { $$ = new_node(COMP_STMT); $$->body[0] = $2; }
                        ;

stmtList                : statement T_SEMICOLON stmtList { $$ = new_node(STATEMENT); $$->body[0] = $1; $$->next = $3; }
                        | statement { $$ = new_node(STATEMENT); $$->body[0] = $1; }
                        ;
 
statement	        : assignStmt { $$ = $1; }
		        | compStmt { $$ = $1; }
		        | ifStmt { $$ = $1; }
		        | whileStmt { $$ = $1; }
		        | forStmt { $$ = $1; }
		        | T_READ T_LEFT_BRACKET exprList T_RIGHT_BRACKET { $$ = new_node(IO_READ); $$->body[0] = $3; }
		        | T_WRITE T_LEFT_BRACKET exprList T_RIGHT_BRACKET { $$ = new_node(IO_WRITE); $$->body[0] = $3; }
                        ;

assignStmt              : T_ID T_ASSIGNMENT expr { $$ = new_node(ASSIGN); $$->body[0] = new_node(IDENTIFIER); $$->body[0]->symbol = get_and_verify_ident_symbol($1); $$->body[2] = $3; check_assign($1, $3); }
                        | T_ID T_LEFT_SQUARE_BRACKET expr T_RIGHT_SQUARE_BRACKET T_ASSIGNMENT expr { $$ = new_node(ASSIGN); $$->body[0] = new_node(IDENTIFIER); $$->body[0]->symbol = get_and_verify_ident_symbol($1); $$->body[1] = $3; $$->body[2] = $6; check_array($1, $3); }
                        ;
 
ifStmt		        : T_IF expr T_THEN statement { $$ = new_node(IF); $$->body[0] = $2; $$->body[1] = $4; check_condition($2); }
                        | T_IF expr T_THEN statement T_ELSE statement { $$ = new_node(IF); $$->body[0] = $2; $$->body[1] = $4; $$->body[2] = $6; }
                        ;
 
whileStmt	        : T_WHILE expr T_DO statement { $$ = new_node(WHILE); $$->body[0] = $2; $$->body[1] = $4; }
                        ;
 
forStmt		        : T_FOR T_ID T_ASSIGNMENT expr toPart expr T_DO statement { $$ = new_node(FOR); $$->body[0] = new_node(IDENTIFIER); $$->body[0]->symbol = get_and_verify_ident_symbol($2); $$->body[1] = $4; $$->body[2] = $5; $$->body[3] = $6; $$->body[4] = $8; check_assign($2, $4); }
                        ;
 
toPart		        : T_TO { $$ = new_node(FOR_TO); }
		        | T_DOWNTO { $$ = new_node(FOR_DOWNTO); }
                        ;
 
expr                    : simpleExpr relOp simpleExpr { $$ = new_node(EXPR); $$->body[0] = $1; $$->body[1] = $2; $$->body[2] = $3; check_illegal_op($1, $2, $3, EXPR_PART_SIMPLE_EXPR, EXPR_PART_SIMPLE_EXPR); }
                        | simpleExpr { $$ = new_node(EXPR); $$->body[0] = $1; }
                        ;

exprList                : expr T_COMMA exprList { $$ = $1; $$->next = $3; }
                        | expr { $$ = $1; }
                        ;

simpleExpr              : simpleExpr addOp term { $$ = new_node(EXPR); $$->body[0] = $1; $$->body[1] = $2; $$->body[2] = $3; check_illegal_op($1, $2, $3, EXPR_PART_SIMPLE_EXPR, EXPR_PART_TERM); }
                        | term { $$ = new_node(EXPR); $$->body[0] = $1; }
                        ;

term                    : term mulOp factor { $$ = new_node(EXPR); $$->body[0] = $1; $$->body[1] = $2; $$->body[2] = $3; check_illegal_op($1, $2, $3, EXPR_PART_TERM, EXPR_PART_FACTOR); }
                        | factor { $$ = new_node(EXPR); $$->body[0] = $1; }
                        ;

factor		        : number { $$ = $1; }
		        | T_FALSE { $$ = new_node(CONST); $$->symbol = symbol_get_or_add_int(_BOOL, 0); }
		        | T_TRUE { $$ = new_node(CONST);  $$->symbol = symbol_get_or_add_int(_BOOL, 1); }
		        | T_ID { $$ = new_node(IDENTIFIER);  $$->symbol = get_and_verify_ident_symbol($1); }
                        | T_ID T_LEFT_SQUARE_BRACKET expr T_RIGHT_SQUARE_BRACKET { $$ = new_node(IDENTIFIER_SUBSCRIPT); $$->body[0] = new_node(IDENTIFIER); $$->body[0]->symbol = get_and_verify_ident_symbol($1); $$->body[1] = $3; check_array($1, $3); }	
		        | T_NOT factor { $$ = new_node(FACTOR_NOT); $$->body[0] = $2; }
		        | T_MINUS factor { $$ = new_node(FACTOR_MINUS); $$->body[0] = $2; }
		        | T_LEFT_BRACKET expr T_RIGHT_BRACKET { $$ = new_node(FACTOR_EXPR); $$->body[0] = $2; }
		        | T_STRING { $$ = new_node(CONST); $$->symbol = symbol_get_or_add_string($1); }
                        ;

number                  : T_NUM_INT { $$ = new_node(CONST); $$->symbol = symbol_get_or_add_int(_INT, $1); }
                        | T_NUM_REAL { $$ = new_node(CONST);  $$->symbol = symbol_get_or_add_real($1); }
                        ;
 		        
relOp		        : T_LESS_THAN { $$ = new_node(OP); $$->op = LT; }
		        | T_LESS_EQUAL_THAN { $$ = new_node(OP); $$->op = LE; }
		        | T_GREATER_THAN { $$ = new_node(OP); $$->op = GT; }
		        | T_GREATER_EQUAL_THAN { $$ = new_node(OP); $$->op = GE; }
		        | T_EQUAL { $$ = new_node(OP); $$->op = EQ; }
		        | T_UNEQUAL { $$ = new_node(OP); $$->op = NE; }
                        ;
 		        
addOp		        : T_PLUS { $$ = new_node(OP); $$->op = PLUS; }
		        | T_MINUS { $$ = new_node(OP); $$->op = MINUS; }
		        | T_OR { $$ = new_node(OP); $$->op = OR; }
                        ;
 		        
mulOp		        : T_STAR { $$ = new_node(OP); $$->op = MUL; }
		        | T_SLASH { $$ = new_node(OP); $$->op = SLASH; }
		        | T_DIV { $$ = new_node(OP); $$->op = DIV; }
		        | T_MOD { $$ = new_node(OP); $$->op = MOD; }
		        | T_AND { $$ = new_node(OP); $$->op = AND; }
                        ;

%%

// For task a)
entry *get_and_verify_ident_symbol(const char *const ident) {
    entry *entry = symbol_get_ident(ident);
    if (entry == NULL) {
	combine_err_msg_1_repl("Semantic error - undeclared variable %s", ident);
    }
    return entry;
}

// For task b)
void declare_vars(node *identListType) {
	node *identNode = identListType->body[0];
	const node *const typeNode = identListType->body[1];
	const entry_type etype = typeNode->body[0] != NULL ? _ARRAY : _SCALAR;
	data_type dtype;
	switch (typeNode->body[2]->type) {
	case SIMPLE_TYPE_INT:
		dtype = _INT;
		break;
	case SIMPLE_TYPE_REAL:
		dtype = _REAL;
		break;
	case SIMPLE_TYPE_BOOL:
		dtype = _BOOL;
		break;
	default:
		fprintf(stderr, "expected simpletype node, but got %d!\n", typeNode->body[2]->type);
		break;
	}

	while (identNode != NULL) {
	    // For task b)
	    entry *entry = symbol_get_ident(identNode->ident_temp);
	    if (entry != NULL) {
		combine_err_msg_1_repl("Semantic error - variable %s is already declared in this scope", identNode->ident_temp);
	    }

	    identNode->symbol = symbol_add_ident(etype, dtype, identNode->ident_temp);

	    identNode = identNode->next;
	}
}

// For task c)
void check_array(const char *const ident, node *index_expr) {
    entry *entry = symbol_get_ident(ident);
    // Check only if entry was created
    if(entry != NULL) {
	entry_type ident_entry_type = entry->etype;
	if(ident_entry_type != _ARRAY) {
		char *err_msg_part_2 = get_entry_type_char(ident_entry_type);
		combine_err_msg_1_repl("Semantic error - identifier is not an array, its entry type is %s", err_msg_part_2);
	    }
	
	    data_type index_expr_type = get_expr_data_type(index_expr, EXPR_PART_EXPR);
	    if(index_expr_type != _INT) {
		char *err_msg_part_2 = get_data_type_char(index_expr_type);
		combine_err_msg_1_repl("Semantic error - wrong index for array, expected INT, but got %s ", err_msg_part_2);
	    }
    }
}

// For task d) 
void check_array_decl(node *number1, node *number2) {
    if(number1->symbol->dtype != _INT){
	combine_err_msg_1_repl("Semantic error - wrong datatype %s for first array index in array declaration", get_data_type_char(number1->symbol->dtype));
    } else if(number1->symbol->symbol.int_val < 0) {
	char err_msg[100];
	sprintf(err_msg, "Semantic error - negative first array index %d in array declaration", number1->symbol->symbol.int_val);
	yyerror(err_msg);
    }
    if(number2->symbol->dtype != _INT){
	combine_err_msg_1_repl("Semantic error - wrong datatype %s for second array index in array declaration", get_data_type_char(number2->symbol->dtype));
    } else if(number2->symbol->symbol.int_val < 0) {
	combine_err_msg_int("Semantic error - negative first array index %d in array declaration", number2->symbol->symbol.int_val);
    }
}

// For task e)
void check_assign(const char *const ident, node *expr) {
    entry *entry = symbol_get_ident(ident);
    // TODO: consider arrays
    if(entry != NULL) {
	data_type ident_data_type = entry->dtype;
	data_type expr_data_type = get_expr_data_type(expr, EXPR_PART_EXPR);
	
	if((ident_data_type != expr_data_type) && 
	   !(ident_data_type == _INT && expr_data_type == _REAL) &&
	   !(ident_data_type == _REAL && expr_data_type == _INT)) {
	    char *err_msg_part2 = get_data_type_char(ident_data_type);
	    char *err_msg_part3 = get_data_type_char(expr_data_type);

	    combine_err_msg_2_repl("Semantic error - incompatible datatypes (%s and %s) in assign statement", err_msg_part2, err_msg_part3);
	}
    }
}

// For task f)
void check_illegal_op(node *left_node, node *op, node *right_node, expr_part expr_part1, expr_part expr_part2) {
    operator op_val = op->op;
    data_type left_node_dtype = get_expr_data_type(left_node, expr_part1);
    data_type right_node_dtype = get_expr_data_type(right_node, expr_part2);
    int illegal_op = 0;
    switch(op_val) {
    case LT:
    case LE:
    case GT:
    case GE:
    case MINUS:
    case MUL:
    case SLASH:
	// Int and real combined
	if(!(
	     (left_node_dtype == _INT || left_node_dtype == _REAL) && (right_node_dtype == _INT || right_node_dtype == _REAL)
	    )
	   ) {
	    illegal_op = 1;
	}
	break;
    case EQ:
    case NE:
	// All datatypes, int and real combined
	if(!(
	     (left_node_dtype == _BOOL && right_node_dtype == _BOOL) ||
	     ((left_node_dtype == _INT || left_node_dtype == _REAL) && (right_node_dtype == _INT || right_node_dtype == _REAL )) ||
	     (left_node_dtype == _STRING && right_node_dtype == _STRING)
	    )
	  ) {
	    illegal_op = 1;
	}
	break;
    case PLUS:
	// String, int and real combined
	if(!(
	     ((left_node_dtype == _INT || left_node_dtype == _REAL) && (right_node_dtype == _INT || right_node_dtype == _REAL )) ||
	     (left_node_dtype == _STRING && right_node_dtype == _STRING)
	    )
	  ) {
	    illegal_op = 1;
	}
	break;
    case OR:
    case AND:
	// Boolean
	if(!(left_node_dtype == _BOOL && right_node_dtype == _BOOL)) {
	    illegal_op = 1;
	}
	break;
    case DIV:
    case MOD:
	// Int
	if(!(left_node_dtype == _INT && right_node_dtype == _INT)) {
	    illegal_op = 1;
	}
	break;
    }

    if(illegal_op != 0){
	char *err_msg_part2 = get_data_type_char(left_node_dtype);
	char *err_msg_part3 = get_data_type_char(right_node_dtype);
	char *err_msg_part4 = get_op_char(op_val);
	combine_err_msg_3_repl("Semantic error - wrong operands (%s and %s) for operator %s", err_msg_part2, err_msg_part3, err_msg_part4);
    }
}

// For task g)
check_condition(node *expr){
    data_type expr_dtype = get_expr_data_type(expr, EXPR_PART_EXPR);
    if(expr_dtype != _BOOL){
	char *err_msg_part2 = get_data_type_char(expr_dtype);
	combine_err_msg_1_repl("Semantic error - condition datatype is not BOOL (its %s)", err_msg_part2);
    };
}

// For task c), e), f) and g)
// TODO: real and integer combinations
data_type get_expr_data_type(node *expr, expr_part part) {
    switch(part) {
    case EXPR_PART_EXPR:
	if(expr->body[1] == NULL) {
	    return get_expr_data_type(expr->body[0], EXPR_PART_SIMPLE_EXPR);
	}
	else {
	    data_type simple_expr1_type = get_expr_data_type(expr->body[0], EXPR_PART_SIMPLE_EXPR);
	    data_type simple_expr2_type = get_expr_data_type(expr->body[2], EXPR_PART_SIMPLE_EXPR);
	    if(simple_expr1_type != simple_expr2_type) {
		char *err_msg_part_2 = get_data_type_char(simple_expr1_type);
		char *err_msg_part_3 = get_data_type_char(simple_expr2_type);
		combine_err_msg_2_repl("Semantic error - expression has incompatible data types (%s and %s)", err_msg_part_2, err_msg_part_3);
	    }
	    // relop expressions always returns bool
	    return _BOOL;
	}
    case EXPR_PART_SIMPLE_EXPR:
	if(expr->body[1] == NULL) {
	    return get_expr_data_type(expr->body[0], EXPR_PART_TERM);
	}
	else {
	    data_type simple_expr_type = get_expr_data_type(expr->body[0], EXPR_PART_SIMPLE_EXPR);
	    data_type term_type = get_expr_data_type(expr->body[2], EXPR_PART_TERM);
	    if(simple_expr_type != term_type) {
		char *err_msg_part_2 = get_data_type_char(simple_expr_type);
		char *err_msg_part_3 = get_data_type_char(term_type);
		combine_err_msg_2_repl("Semantic error - expression has incompatible data types (%s and %s)", err_msg_part_2, err_msg_part_3);
		return _ERROR;
	    }
	    return simple_expr_type;
	}
    case EXPR_PART_TERM:
	if(expr->body[1] == NULL) {
	    return get_expr_data_type(expr->body[0], EXPR_PART_FACTOR);
	}
	else {
	    data_type term_type = get_expr_data_type(expr->body[0], EXPR_PART_TERM);
	    data_type factor_type = get_expr_data_type(expr->body[2], EXPR_PART_FACTOR);
	    if(term_type != factor_type) {
		char *err_msg_part_2 = get_data_type_char(term_type);
		char *err_msg_part_3 = get_data_type_char(factor_type);
		combine_err_msg_2_repl("Semantic error - expression has incompatible data types (%s and %s)", err_msg_part_2, err_msg_part_3);
		return _ERROR;
	    }
	    return term_type;
	}
    case EXPR_PART_FACTOR:
	switch(expr->type) {
	case CONST:
	case IDENTIFIER:
	    if(expr->symbol == NULL){ // error was already handled, so just return error
		return _ERROR;
	    }
	    return expr->symbol->dtype;
	case IDENTIFIER_SUBSCRIPT:
	    if(expr->body[0]->symbol == NULL){ // error was already handled, so just return error
		return _ERROR;
	    }
	    return expr->body[0]->symbol->dtype;
	case FACTOR_NOT: {
	    data_type factor_type = get_expr_data_type(expr->body[0], EXPR_PART_FACTOR);
	    if(factor_type != _BOOL) {
		char *err_msg_part_2 = get_data_type_char(factor_type);
		combine_err_msg_1_repl("Semantic error - expected data type bool in front of NOT, but got %s", err_msg_part_2);
		return _ERROR;
	    }
	    return factor_type;
	}
	case FACTOR_MINUS: {
	    data_type factor_type = get_expr_data_type(expr->body[0], EXPR_PART_FACTOR);
	    if(factor_type != _INT && factor_type != _REAL) {
		char *err_msg_part_2 = get_data_type_char(factor_type);
		combine_err_msg_1_repl("Semantic error - expected data type int or real in front of \"-\", but got %s", err_msg_part_2);
		return _ERROR;
	    }
	    return factor_type;
	}
	case FACTOR_EXPR: 
	    return get_expr_data_type(expr->body[0], EXPR_PART_EXPR);
	}
    }
}

void yyerror(char *s) {
    extern int yylineno;
    fprintf(stderr, "%s (line %d)\n", s, yylineno);
    parser_err_ctr++;
}

void combine_err_msg_1_repl(char const *part1, char const* part2) {
    char err_msg[100];
    sprintf(err_msg, part1, part2);
    yyerror(err_msg);
}

void combine_err_msg_2_repl(char const *part1, char const* part2, char const* part3) {
    char err_msg[100];
    sprintf(err_msg, part1, part2, part3);
    yyerror(err_msg);
}

void combine_err_msg_3_repl(char const *part1, char const* part2, char const* part3, char const* part4) {
    char err_msg[100];
    sprintf(err_msg, part1, part2, part3, part4);
    yyerror(err_msg);
}


void combine_err_msg_int(char const *part1, int part2) {
    char err_msg[100];
    sprintf(err_msg, part1, part2);
    yyerror(err_msg);
}

int main() {
    int status = yyparse();
    if ((!status) && (parser_err_ctr == 0)) {
        printf("--> parsing successful <--\n");
        printf("--> print parsed program: <--\n\n");

	print_node(root_node);

	printf("--> end of parsed program <--\n");
    }
    else {
	printf("Found %u during parsing\n", parser_err_ctr);
    }

    printf("--> printing symbol table <--\n\n");
    symbol_print_table();
    printf("\n--> end of symbol table <--\n");    

    free_node(root_node);
    symbol_free();

    return status;
}
