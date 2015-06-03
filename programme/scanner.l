%{
#include <stdio.h>
#include "parser.tab.h"

void yyerror(char *);
char message[100];
%}

%option noyywrap
%option yylineno

%x COMMENT

WHITESPACE [ \t\n]
INTEGER [0-9]+
REAL [0-9]*\.[0-9]+
ID [a-zA-Z_][a-zA-Z_0-9]*

A [aA]
B [bB]
C [cC]
D [dD]
E [eE]
F [fF]
G [gG]
H [hH]
I [iI]
J [jJ]
K [kK]
L [lL]
M [mM]
N [nN]
O [oO]
P [pP]
Q [qQ]
R [rR]
S [sS]
T [tT]
U [uU]
V [vV]
W [wW]
X [xX]
Y [yY]
Z [zZ]

%%

"{"                  BEGIN(COMMENT);
<COMMENT>[^}\n]+     ;
<COMMENT>\n          ;
<COMMENT><<EOF>>     sprintf(message, "Lexical Error (Line: %d) EOF in comment", yylineno); yyerror(message);
<COMMENT>"}"         BEGIN(INITIAL);

    /* Keywords */
{P}{R}{O}{G}{R}{A}{M}			return T_PROGRAM;
{V}{A}{R}				return T_VAR;
{I}{N}{T}{E}{G}{E}{R}			return T_INTEGER;
{A}{R}{R}{A}{Y}				return T_ARRAY;
{O}{F}					return T_OF;
{R}{E}{A}{L}				return T_REAL;
{B}{O}{O}{L}{E}{A}{N}                   return T_BOOLEAN;
{B}{E}{G}{I}{N}				return T_BEGIN;
{W}{H}{I}{L}{E}				return T_WHILE;
{D}{O}					return T_DO;
{I}{F}					return T_IF;
{T}{H}{E}{N}				return T_THEN;
{E}{L}{S}{E}				return T_ELSE;
{E}{N}{D}				return T_END;
{F}{O}{R}				return T_FOR;
{T}{O}					return T_TO;
{D}{O}{W}{N}{T}{O}                      return T_DOWNTO;
{R}{E}{A}{D}                            return T_READ;
{W}{R}{I}{T}{E}                         return T_WRITE;
{D}{I}{V}                               return T_DIV;
{M}{O}{D}                               return T_MOD;
{A}{N}{D}                               return T_AND;
{O}{R}                                  return T_OR;
{T}{R}{U}{E}                            return T_TRUE;
{F}{A}{L}{S}{E}                         return T_FALSE;
{N}{O}{T}                               return T_NOT;
					       
   /* Special symbols */		       
";"					return T_SEMICOLON;
","					return T_COMMA;
":="					return T_ASSIGNMENT;
":"					return T_COLON;
"["					return T_LEFT_SQUARE_BRACKET;
"]"					return T_RIGHT_SQUARE_BRACKET;
".."					return T_DOT_DOT;
"."					return T_DOT;
"("					return T_LEFT_BRACKET;
")"					return T_RIGHT_BRACKET;
"*"					return T_STAR;
"/"					return T_SLASH;
"+"					return T_PLUS;
"-"					return T_MINUS;
"<>"					return T_UNEQUAL;
"<"					return T_LESS_THAN;
">"					return T_GREATER_THAN;
">="|"=>"				return T_GREATER_EQUAL_THAN;
"<="|"=<"				return T_LESS_EQUAL_THAN;
"="					return T_EQUAL;
					       
\"(\\.|[^\\"])*\"|\'(\\.|[^\\'])*\'     { yylval.string = strdup(yytext); return T_STRING; }
					       
    /* Numbers */			       
{INTEGER}                               { yylval.integer = atoi(yytext); return T_NUM_INT; }
{REAL}                                  { yylval.real = atof(yytext); return T_NUM_REAL; }

    /* Identifiers */			       
{ID}                                    { yylval.string = strdup(yytext); return T_ID; }

    /* Whitespace */
{WHITESPACE}+                           ;

    /* Lexical errors */
.                                       sprintf(message, "Lexical Error (Line: %d) Illegal input", yylineno); yyerror(message);

%%
