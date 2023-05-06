%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "lex.yy.c"

typedef struct TypeSpecifier {
    enum {
        INT_TYPE,
        FLOAT_TYPE,
        CHAR_TYPE,
        BOOL_TYPE,
        STRING_TYPE
    } type;
} TypeSpecifier;

typedef struct parameter_declaration {
    char* name;
    char* type;
    struct identifier_list* index_expression;
    struct parameter_declaration* next;
} parameter_declaration_t;

typedef struct ArraySpecifierList {
    int size;
    struct ArraySpecifierList* next;
} ArraySpecifierList;

typedef struct ArrayAssignmentStatement {
    struct IdentifierList* identifier_list;
    struct ArrayIndexList* array_index_list;
    struct Expression* expression;
} ArrayAssignmentStatement;

typedef struct ArrayIndexList {
    int index;
    struct ArrayIndexList* next;
} ArrayIndexList;

typedef struct function_declaration {
    char* name;
    parameter_declaration_t* parameters;
    TypeSpecifier* return_type;
} function_declaration_t;

typedef struct identifier_list {
    char* name;
    struct expression* index_expression;
    struct identifier_list* next;
} identifier_list_t;

typedef struct variable_declaration {
    identifier_list_t* identifier_list;
    TypeSpecifier* type_specifier;
    ArraySpecifierList* array_specifier_list;
} variable_declaration_t;

typedef struct array_list {
    int value;
    struct array_list* next;
} array_list_t;

typedef struct expression {
    enum {
        NUMBER_EXPR,
        IDENTIFIER_EXPR,
        POINTER_EXPR,
        ADD_EXPR,
        SUB_EXPR,
        MUL_EXPR,
        DIV_EXPR,
        PTR_ADD_EXPR,
        PTR_SUB_EXPR,
        PTR_LT_EXPR,
        PTR_GT_EXPR,
        PTR_LE_EXPR,
        PTR_GE_EXPR,
        PTR_EQ_EXPR,
        PTR_NE_EXPR
    } type;
    union {
        int number_value;
        char* identifier_value;
        struct expression* pointer_value;
        struct {
            struct expression* left;
            struct expression* right;
        } binary_expr;
        struct {
            struct expression* left;
            struct expression* right;
        } pointer_binary_expr;
    } u;
} expression_t;

variable_declaration_t* create_variable_declaration(identifier_list_t* identifier_list, TypeSpecifier* type_specifier, ArraySpecifierList* array_specifier_list) {
    variable_declaration_t* var_decl = (variable_declaration_t*) malloc(sizeof(variable_declaration_t));
    var_decl->identifier_list = identifier_list;
    var_decl->type_specifier = type_specifier;
    var_decl->array_specifier_list = array_specifier_list;
    return var_decl;
}

ArrayAssignmentStatement* create_array_assignment_statement(struct IdentifierList* identifier_list, ArrayIndexList* array_index_list, struct Expression* expression) {
    ArrayAssignmentStatement* arr_assign_stmt = (ArrayAssignmentStatement*) malloc(sizeof(ArrayAssignmentStatement));
    arr_assign_stmt->identifier_list = identifier_list;
    arr_assign_stmt->array_index_list = array_index_list;
    arr_assign_stmt->expression = expression;
    return arr_assign_stmt;
}

TypeSpecifier* create_type_specifier(TypeSpecifier* type_specifier) {
    TypeSpecifier* specifier = malloc(sizeof(TypeSpecifier));
    specifier->type = type_specifier->type;
    return specifier;
}

char* convert_type_specifier(TypeSpecifier* type_specifier) {
    switch (type_specifier->type) {
        case INT_TYPE:
            return "int";
        case FLOAT_TYPE:
            return "float";
        case CHAR_TYPE:
            return "char";
        case BOOL_TYPE:
            return "bool";
        case STRING_TYPE:
            return "string";
        default:
            return "";
    }
}

function_declaration_t* create_function(char* name, parameter_declaration_t* parameters, int* return_type, ...) {
    function_declaration_t* func = malloc(sizeof(function_declaration_t));
    func->name = strdup(name);
    func->parameters = parameters;
    func->return_type = return_type;
    return func;
}


TypeSpecifier* create_string_type()
{
    TypeSpecifier* specifier = create_type_specifier(&(TypeSpecifier){STRING_TYPE});
    return specifier;
}


ArrayAssignmentStatement* create_array_assignment(struct IdentifierList* identifier_list, ArrayIndexList* array_index_list, struct Expression* expression) {
    ArrayAssignmentStatement* arr_assign_stmt = create_array_assignment_statement(identifier_list, array_index_list, expression);
    return arr_assign_stmt;
}
%}


%token COMMENT FUNCTION IDENTIFIER INT FLOAT BOOL
%token CHAR VOID NUMBER VAR IF WHILE RETURN
%token ARROW STRING INTEGER_LITERAL ELSE

%%

program:
    declaration_list 


declaration_list: /* пусто */
                | declaration_list declaration

declaration:
    function_declaration 
    | variable_declaration 



function_declaration:
    FUNCTION IDENTIFIER '(' parameter_declaration_list ')' ':' type_specifier compound_statement
    {
        $$ = create_function($2, $4, &($6->type));
    }
    ;

parameter_declaration: 
    IDENTIFIER ARROW identifier_list ':' type_specifier

parameter_declaration_list:
              | parameter_declaration
              | parameter_declaration_list ',' parameter_declaration
              ;

identifier_list:
                | IDENTIFIER '[' expression ']'
                | identifier_list ',' IDENTIFIER '[' expression ']'

type_specifier: INT
              {
                $$ = &(TypeSpecifier){INT_TYPE};
              }
              | FLOAT
              {
                $$ = &(TypeSpecifier){FLOAT_TYPE};
              }
              | CHAR
              {
                $$ = &(TypeSpecifier){CHAR_TYPE};
              }
              | BOOL
              {
                $$ = &(TypeSpecifier){BOOL_TYPE};
              }
              | STRING
              {
                $$ = create_string_type();
              }

variable_declaration:
    VAR identifier_list ':' type_specifier array_specifier_list ';'
    {
        $$ = create_variable_declaration($2, $4, $5);
    }
    | VAR identifier_list ':' STRING ';'
    {
        $$ = create_variable_declaration($2, create_string_type());
    }
    
compound_statement: '{' statement_list '}'

statement_list: /* пусто */
              | statement
              | statement_list statement

array_specifier_list:
    /* пусто */ 
    | array_specifier_list array_specifier

array_specifier:
    '[' INTEGER_LITERAL ']'

statement:
    expression ';'
    | compound_statement
    | if_statement
    | while_statement
    | return_statement
    | assignment_statement
    | variable_declaration_statement
    | function_call_statement
    | array_assignment_statement

array_assignment_statement:
    identifier_list array_index_list '=' expression ';'
    {
        $$ = create_array_assignment($1, $2, $4);
    }

array_index_list:
    /* пусто */ 
    | array_index_list array_index

array_index:
    '[' expression ']'

expression:
    NUMBER
    | IDENTIFIER
    | pointer_expression
    | '(' expression ')'
    | expression '+' expression
    | expression '-' expression
    | expression '*' expression
    | expression '/' expression
    | pointer_arithmetic_expression
    | pointer_comparison_expression
    ;

if_statement: IF '(' expression ')' statement
            | IF '(' expression ')' statement ELSE statement
            ;

while_statement: WHILE '(' expression ')' statement
                ;

return_statement: RETURN expression ';'

assignment_statement: IDENTIFIER '=' expression ';'
                     | pointer_expression '=' expression ';'
                     ;

variable_declaration_statement: VAR parameter_declaration ';'

function_call_statement: IDENTIFIER '(' argument_list ')' ';'

argument_list: /* пусто */
             | expression
             | argument_list ',' expression

pointer_expression: '&' IDENTIFIER
                  | '&' IDENTIFIER '[' expression ']'
                  ;

pointer_arithmetic_expression: pointer_expression '+' expression
                             | pointer_expression '-' expression
                             ;

pointer_comparison_expression: pointer_expression '<' pointer_expression
                              | pointer_expression '>' pointer_expression
                              | pointer_expression '<' '=' pointer_expression
                              | pointer_expression '>' '=' pointer_expression
                              | pointer_expression '=' '=' pointer_expression
                              | pointer_expression '!' '=' pointer_expression
                              ;

%%

int yyerror(const char *msg) {
    fprintf(stderr, "Error: %s\n", msg);
    return 0;
}

int main() {
    yyparse();
    return 0;
}



