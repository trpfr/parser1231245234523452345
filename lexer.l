%{
#include <stdio.h>
#include <string.h>
%}

%%

"bool" { printf("BOOL\n"); }
"char" { printf("CHAR\n"); }
"int" { printf("INT\n"); }
"real" { printf("REAL\n"); }
"string" { printf("STRING\n"); }
"int*" { printf("INT_POINTER\n"); }
"char*" { printf("CHAR_POINTER\n"); }
"real*" { printf("REAL_POINTER\n"); }
"if" { printf("IF\n"); }
"else" { printf("ELSE\n"); }
"while" { printf("WHILE\n"); }
"for" { printf("FOR\n"); }
"var" { printf("VAR\n"); }
"arg->" { printf("ARG_POINTER\n"); }
"function" { printf("FUNCTION\n"); }
"return" { printf("RETURN\n"); }
"null" { printf("NULL\n"); }
"void" { printf("VOID\n"); }
"do" { printf("DO\n"); }

&& { printf("AND\n"); }
\/ { printf("DIVIDE\n"); }
"=" { printf("ASSIGN\n"); }
"==" { printf("EQUALS\n"); }
">" { printf("GREATER_THAN\n"); }
">=" { printf("GREATER_THAN_EQUALS\n"); }
"<" { printf("LESS_THAN\n"); }
"<=" { printf("LESS_THAN_EQUALS\n"); }
"-" { printf("MINUS\n"); }
"!" { printf("NOT\n"); }
"!=" { printf("NOT_EQUALS\n"); }
"||" { printf("OR\n"); }
"+" { printf("PLUS\n"); }
"*" { printf("MULTIPLY\n"); }
"&" { printf("REFERENCE\n"); }
":" { printf("COLON\n"); }


"true" { printf("BOOLEAN_LITERAL\n"); }
"false" { printf("BOOLEAN_LITERAL\n"); }

\'[a-zA-Z0-9]\' { printf("CHAR_LITERAL\n"); }

[1-9][0-9]*|0[Xx][0-9A-Fa-f]+ { printf("INTEGER_LITERAL\n"); }

[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)? { printf("REAL_LITERAL\n"); }

\"([^\\\n]|(\\.))*?\" { printf("STRING_LITERAL\n"); }

[a-zA-Z][a-zA-Z0-9_]* { printf("IDENTIFIER\n"); }

\<null\>    { printf("NULL_POINTER\n"); }

";" { printf("SEMICOLON\n"); }
"," { printf("COMMA\n"); }
"|" { printf("PIPE\n"); }
"{" { printf("LEFT_BRACE\n"); }
"}" { printf("RIGHT_BRACE\n"); }
"(" { printf("LEFT_PARENTHESIS\n"); }
")" { printf("RIGHT_PARENTHESIS\n"); }
"[" { printf("LEFT_BRACKET\n"); }
"]" { printf("RIGHT_BRACKET\n"); }

"/*"([^*]|[*][^/])*"*/" { /* do nothing */ }

[ \t\n] { /* ignore whitespace */ }

. { printf("UNKNOWN_TOKEN\n"); }

%%

int yywrap(void) {
    return 1;
}

int main(int argc, char **argv) {
    yylex();
    return 0;
}

