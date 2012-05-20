/* description: Parses end executes mathematical expressions. */

/* lexical grammar */
%lex
%%

"auto"                return 'AUTO'
"break"               return 'BREAK'
"case"                return 'CASE'
"char"                return 'CHAR'
"const"               return 'CONST'
"continue"            return 'CONTINUE'
"default"             return 'DEFAULT'
"do"                  return 'DO'
"double"              return 'DOUBLE'
"else"                return 'ELSE'
"enum"                return 'ENUM'
"extern"              return 'EXTERN'
"float"               return 'FLOAT'
"for"                 return 'FOR'
"goto"                return 'GOTO'
"if"                  return 'IF'
"inline"              return 'INLINE'
"int"                 return 'INT'
"long"                return 'LONG'
"register"            return 'REGISTER'
"restrict"            return 'RESTRICT'
"return"              return 'RETURN'
"short"               return 'SHORT'
"signed"              return 'SIGNED'
"sizeof"              return 'SIZEOF'
"static"              return 'STATIC'
"struct"              return 'STRUCT'
"switch"              return 'SWITCH'
"typedef"             return 'TYPEDEF'
"union"               return 'UNION'
"unsigned"            return 'UNSIGNED'
"void"                return 'VOID'
"volatile"            return 'VOLATILE'
"while"               return 'WHILE'
"_Bool"               return '_BOOL'
"_Complex"            return '_COMPLEX'
"..."                 return 'ELLIPSIS'
">>="                 return 'RIGHT_ASSIGN'
"<<="                 return 'LEFT_ASSIGN'
"+="                  return 'ADD_ASSIGN'
"-="                  return 'SUB_ASSIGN'
"*="                  return 'MUL_ASSIGN'
"/="                  return 'DIV_ASSIGN'
"%="                  return 'MOD_ASSIGN'
"&="                  return 'AND_ASSIGN'
"^="                  return 'XOR_ASSIGN'
"|="                  return 'OR_ASSIGN'
">>"                  return 'RIGHT_OP'
"<<"                  return 'LEFT_OP'
"++"                  return 'INC_OP'
"--"                  return 'DEC_OP'
"->"                  return 'PTR_OP'
"&&"                  return 'AND_OP'
"||"                  return 'OR_OP'
"<="                  return 'LE_OP'
">="                  return 'GE_OP'
"=="                  return 'EQ_OP'
"!="                  return 'NE_OP'
";"                   return ';'
"{"                   return '{'
"}"                   return '}'
","                   return ','
":"                   return ':'
"="                   return '='
"("                   return '('
")"                   return ')'
"["                   return '['
"]"                   return ']'
"."                   return '.'
"&"                   return '&'
"!"                   return '!'
"~"                   return '~'
"-"                   return '-'
"+"                   return '+'
"*"                   return '*'
"/"                   return '/'
"%"                   return '%'
"<"                   return '<'
">"                   return '>'
"^"                   return '^'
"|"                   return "|"
"?"                   return "?"
\s+                   /* skip whitespace */
[a-zA-Z_][a-zA-Z_0-9]* return 'IDENTIFIER'
"0"[xX][a-fA-F0-9]+[uUlL]?   return 'INTEGER'
[0-9]+[eE][-+]?[0-9]+[fFlL]? return 'FLOATING'
[0-9]*"."[0-9]+([eE][-+]?[0-9]+)?[fFlL]? return 'FLOATING'
[0-9]+"."[0-9]*([eE][-+]?[0-9]+)?[fFlL]? return 'FLOATING'
[0-9]+                return 'INTEGER'
"L"?"'"(\\.|[^\\'])+"'" return 'CHARACTER'
"L"?"\""(\\.|[^\\"])*"\"" return 'STRING_LITERAL'
<<EOF>>               return 'EOF'

/lex

/* operator associations and precedence */

%start start

%% /* language grammar */

/* A.2.1 Expressions */

constant
    : INTEGER
      {
        $$ = new Object();
        $$.node_type = "int";
        $$.value = $1;
      }
    | FLOATING
      {
        $$ = new Object();
        $$.node_type = "float";
        $$.value = $1;
      }
    | CHARACTER
      {
        $$ = new Object();
        $$.node_type = "char";
        $$.value = $1;
      }
    // ENUMERATION TODO ???
    ;

primary_expression
    : IDENTIFIER
      {
        $$ = new Object();
        $$.node_type = "identifier";
        $$.expr = $1;
      }
    | constant
      {
        $$ = new Object();
        $$.node_type = "constant";
        $$.value = $1;
      }
    | STRING_LITERAL
      {
        $$ = new Object();
        $$.node_type = "string_literal";
        $$.expr = $1;
      }
    | '(' expression ')' { $$ = $2; }
    ;

postfix_expression
    : primary_expression
    | postfix_expression '[' expression ']'
      {
        $$ = new Object();
        $$.node_type = 'unary*';
        $$.target = new Object();
        $$.target.node_type = "+";
        $$.target.targets = [$1, $3];
      }
    | postfix_expression '(' ')'
      {
        $$ = new Object();
        $$.node_type = "function_call";
        $$.func = $1;
        $$.args = [];
      }
    | postfix_expression '(' argument_expression_list ')'
      {
        $$ = new Object();
        $$.node_type = "function_call"
        $$.func = $1;
        $$.args = $3;
      }
    | postfix_expression '.' IDENTIFIER
      {
        $$ = new Object();
        $$.node_type = ".";
        $$.targets = [$1, $3];
      }
    | postfix_expression PTR_OP IDENTIFIER
      {
        var inner = new Object();
        inner.node_type = 'unary*';
        inner.targets = [$1];
        $$ = new Object();
        $$.node_type = ".";
        $$.targets = [inner, $3];
      }
    | postfix_expression INC_OP
      {
        $$ = new Object();
        $$.node_type = "post++";
        $$.target = $1;
      }
    | postfix_expression DEC_OP
      {
        $$ = new Object();
        $$.node_type = "post--";
        $$.target = $1;
      }
    ;

argument_expression_list
    : assignment_expression { $$ = [$1]; }
    | argument_expression_list ',' assignment_expression
      {
        $1.push($3);
        $$ = $1;
      }
    ;

unary_expression
    : postfix_expression
    | INC_OP unary_expression
      {
        $$ = new Object();
        $$.node_type = "pre++";
        $$.target = $2;
      }
    | DEC_OP unary_expression
      {
        $$ = new Object();
        $$.node_type = "pre--";
        $$.target = $2;
      }
    | unary_operator cast_expression
      {
        $$ = new Object();
        $$.node_type = "unary" + $1;
        $$.target = $2;
      }
    | SIZEOF unary_expression
      {
        $$ = new Object();
        $$.node_type = "sizeof";
        $$.target = $2;
      }
    | SIZEOF '(' type_name ')'
      {
        $$ = new Object();
        $$.node_type = "sizeof";
        $$.target = $3;
      }
    ;

unary_operator
    : '&'
    | '+'
    | '*'
    | '-'
    | '~'
    | '!'
    ;

cast_expression
    : unary_expression
    | '(' type_name ')' cast_expression
      {
        $$ = new Object();
        $$.node_type = "typecast";
        $$.type = $2; // TODO: this might best be best hammered into a node_type="type" object
        $$.target = $4;
      }
    ;

multiplicative_expression
    : cast_expression
    | multiplicative_expression '*' cast_expression
      {
        $$ = new Object();
        $$.node_type = "*";
        $$.targets = [$1, $3];
      }
    | multiplicative_expression '/' cast_expression
      {
        $$ = new Object();
        $$.node_type = "/";
        $$.targets = [$1, $3];
      }
    | multiplicative_expression '%' cast_expression
      {
        $$ = new Object();
        $$.node_type = "%";
        $$.targets = [$1, $3];
      }
    ;

additive_expression
    : multiplicative_expression
    | additive_expression '+' multiplicative_expression
      {
        $$ = new Object();
        $$.node_type = "+";
        $$.targets = [$1, $3];
      }
    | additive_expression '-' multiplicative_expression
      {
        $$ = new Object();
        $$.node_type = "-";
        $$.targets = [$1, $3];
      }
    ;

shift_expression
    : additive_expression
    | shift_expression LEFT_OP additive_expression
      {
        $$ = new Object();
        $$.node_type = "<<";
        $$.targets = [$1, $3];
      }
    | shift_expression RIGHT_OP additive_expression
      {
        $$ = new Object();
        $$.node_type = ">>";
        $$.targets = [$1, $3];
      }
    ;

relational_expression
    : shift_expression
    | relational_expression '<' shift_expression
      {
        $$ = new Object();
        $$.node_type = "<";
        $$.targets = [$1, $3];
      }
    | relational_expression '>' shift_expression
      {
        $$ = new Object();
        $$.node_type = ">";
        $$.targets = [$1, $3];
      }
    | relational_expression LE_OP shift_expression
      {
        $$ = new Object();
        $$.node_type = "<=";
        $$.targets = [$1, $3];
      }
    | relational_expression GE_OP shift_expression
      {
        $$ = new Object();
        $$.node_type = ">=";
        $$.targets = [$1, $3];
      }
    ;

equality_expression
    : relational_expression
    | equality_expression EQ_OP relational_expression
      {
        $$ = new Object();
        $$.node_type = "==";
        $$.targets = [$1, $3];
      }
    | equality_expression NE_OP relational_expression
      {
        $$ = new Object();
        $$.node_type = "!=";
        $$.targets = [$1, $3];
      }
    ;

and_expression
    : equality_expression
    | and_expression '&' equality_expression
      {
        $$ = new Object();
        $$.node_type = "&";
        $$.targets = [$1, $3];
      }
    ;

exclusive_or_expression
    : and_expression
    | exclusive_or_expression '^' and_expression
      {
        $$ = new Object();
        $$.node_type = "^";
        $$.targets = [$1, $3];
      }
    ;

inclusive_or_expression
    : exclusive_or_expression
    | inclusive_or_expression '|' exclusive_or_expression
      {
        $$ = new Object();
        $$.node_type = "|";
        $$.targets = [$1, $3];
      }
    ;

logical_and_expression
    : inclusive_or_expression
    | logical_and_expression AND_OP inclusive_or_expression
      {
        $$ = new Object();
        $$.node_type = "&&";
        $$.targets = [$1, $3];
      }
    ;

logical_or_expression
    : logical_and_expression
    | logical_or_expression OR_OP logical_and_expression
      {
        $$ = new Object();
        $$.node_type = "||";
        $$.targets = [$1, $3];
      }
    ;

conditional_expression
    : logical_or_expression
    | logical_or_expression '?' expression ':' conditional_expression
      {
        $$ = new Object();
        $$.node_type = "?";
        $$.targets = [$1, $3, $5];
      }
    ;

assignment_expression
    : conditional_expression
    | unary_expression assignment_operator assignment_expression
      {
        $$ = new Object();
        $$.node_type = $2;
        $$.targets = [$1, $3];
      }
    ;

assignment_operator
    : '='
    | MUL_ASSIGN
    | DIV_ASSIGN
    | MOD_ASSIGN
    | ADD_ASSIGN
    | SUB_ASSIGN
    | LEFT_ASSIGN
    | RIGHT_ASSIGN
    | AND_ASSIGN
    | XOR_ASSIGN
    | OR_ASSIGN
    ;

expression
    : assignment_expression
      {
        $$ = new Object();
        $$.node_type = "expression";
        $$.seqs = [$1];
      }
    | expression ',' assignment_expression
      {
        $1.seqs.push($3);
        $$ = $1;
      }
    ;

constant_expression
    : conditional_expression
    ;

/* A.2.2 Declarations */

declaration
    : declaration_specifiers ';'
      {
        $$ = new Object();
        $$.node_type = "declaration";
        $$.type = $1;
        $$.name = '';
        $$.value = null;
      }
    | declaration_specifiers init_declarator_list ';'
      {
        if(typeof $2.node_type !== "undefined") throw { message: "what" };
        $2.map(function(x) {
          x.node_type = "declaration";
          x.type = copy_type($1);
          if(typeof x.name.node_type !== "undefined") {
            if(x.name.node_type !== "pointer_declarator") {
              throw { message: "Unsupported declarator syntax in declaration while generating AST. (1)" };
            }
            x.type.pointer = x.name.pointer;
            x.name = x.name.direct_decl;
            if(typeof x.name !== "string") {
              throw { message: "Unsupported declarator syntax in declaration while generating AST. (2)" };
            }
          }
        });
        $$ = $2;

      }
    ;

declaration_specifiers
    : storage_class_specifier
      {
        $$ = new Object();
        $$.node_type = "type";
        $$.base_type = [];
        $$.storage = [$1];
        $$.qualifiers = [];
      }
    | storage_class_specifier declaration_specifiers
      {
        $2.storage.push($1);
        $$ = $2;
      }
    | type_specifier
      {
        $$ = new Object();
        $$.node_type = "type";
        $$.base_type = [$1];
        $$.storage = [];
        $$.qualifiers = [];
      }
    | type_specifier declaration_specifiers
      {
        $2.base_type.push($1);
        $$ = $2;
      }
    | type_qualifier
      {
        $$ = new Object();
        $$.node_type = "type";
        $$.base_type = [];
        $$.storage = [];
        $$.qualifiers = [$1];
      }
    | type_qualifier declaration_specifiers
      {
        $2.qualifiers = $2.qualifiers.concat($1);
        $$ = $2;
      }
    | function_specifier
      {
        $$ = new Object();
        $$.node_type = "type";
        $$.base_type = [];
        $$.storage = [];
        $$.qualifiers = [];
      }
    | function_specifier declaration_specifiers
      {
        $$ = $2;
      }
    ;

init_declarator_list
    : init_declarator { $$ = new Array(); $$.push($1); }
    | init_declarator_list ',' init_declarator
      {
        $1.push($3);
        $$ = $1;
      }
    ;

init_declarator
    : declarator
      {
        $$ = new Object();
        $$.name = $1;
        $$.value = null;
      }
    | declarator '=' initializer
      {
        $$ = new Object();
        $$.name = $1;
        $$.value = $3;
      }
    ;

storage_class_specifier
    : TYPEDEF
    | EXTERN
    | STATIC
    | AUTO
    | REGISTER
    ;

type_specifier
    : VOID
    | CHAR
    | SHORT
    | INT
    | LONG
    | FLOAT
    | DOUBLE
    | SIGNED
    | UNSIGNED
    | _BOOL
    | _COMPLEX
    | struct_or_union_specifier
    | enum_specifier
    //| typedef_name
    ;

struct_or_union_specifier
    : struct_or_union '{' struct_declaration_list '}'
      {
        $$ = new Object();
        $$.node_type = $1;
        $$.name = null;
        $$.decls = $3;
      }
    | struct_or_union IDENTIFIER '{' struct_declaration_list '}'
      {
        $$ = new Object();
        $$.node_type = $1;
        $$.name = $2;
        $$.decls = $4;
      }
    | struct_or_union IDENTIFIER
      {
        $$ = new Object();
        $$.node_type = $1;
        $$.name = null;
        $$.decls = null;
        $$.name = $2;
      }
    ;

struct_or_union
    : STRUCT
    | UNION
    ;

struct_declaration_list
    : struct_declaration { $$ = [$1]; }
    | struct_declaration_list struct_declaration
      {
        $1.push($2);
        $$ = $1;
      }
    ;

struct_declaration
    : specifier_qualifier_list struct_declarator_list ';'
      {
        $$ = new Object();
        $$.node_type = "struct_field";
        $$.members_type = $1;
        $$.members = $2;
      }
    ;

specifier_qualifier_list
    : type_specifier specifier_qualifier_list
      {
        $$ = [$1];
        $$ = $$.concat($2);
      }
    | type_specifier { $$ = [$1]; }
    | type_qualifier specifier_qualifier_list
      {
        $$ = [$1];
        $$ = $$.concat($2);
      }
    | type_qualifier { $$ = [$1]; }
    ;

struct_declarator_list
    : struct_declarator { $$ = [$1]; }
    | struct_declarator_list ',' struct_declarator
      {
        $1.push($3);
        $$ = $1;
      }
    ;

struct_declarator
    : declarator
    | ':' constant_expression
    {
      $$ = new Object();
      $$.node_type = "pad_width";
      $$.decl = null;
      $$.width = $2;
    }
    | declarator ':' constant_expression
    {
      $$ = new Object();
      $$.node_type = "bitfield";
      $$.decl = $1;
      $$.width = $2;
    }
    ;

enum_specifier
    : ENUM '{' enumerator_list '}'
      {
        $$ = new Object();
        $$.node_type = "enum";
        $$.name = null;
        $$.list = $3;
      }
    | ENUM IDENTIFIER '{' enumerator_list '}'
      {
        $$ = new Object();
        $$.node_type = "enum";
        $$.name = $2;
        $$.list = $4;
      }
    | ENUM '{' enumerator_list ',' '}'
      {
        $$ = new Object();
        $$.node_type = "enum";
        $$.name = null;
        $$.list = $3;
      }
    | ENUM IDENTIFIER '{' enumerator_list ',' '}'
      {
        $$ = new Object();
        $$.node_type = "enum";
        $$.name = $2;
        $$.list = $4;
      }
    | ENUM IDENTIFIER
      {
        $$ = new Object();
        $$.node_type = "enum";
        $$.name = $2;
        $$.list = [];
      }
    ;

enumerator_list
    : enumerator { $$ = [$1]; }
    | enumerator_list ',' enumerator
      {
        $1.push($3);
        $$ = $1;
      }
    ;

enumerator
    : IDENTIFIER
      {
        $$ = new Object();
        $$.node_type = "enum_entry";
        $$.name = $1;
        $$.data = null;
      }
    | IDENTIFIER '=' constant_expression
      {
        $$ = new Object();
        $$.node_type = "enum_fixed_entry";
        $$.name = $1;
        $$.data = $3;
      }
    ;

type_qualifier
    : CONST
    | RESTRICT
    | VOLATILE
    ;

function_specifier
    : INLINE
    ;

declarator
    : pointer direct_declarator
    {
      $$ = new Object();
      $$.node_type = "pointer_declarator";
      $$.pointer = $1;
      $$.direct_decl = $2;
    }
    | direct_declarator
    ;

direct_declarator
    : IDENTIFIER
    | '(' declarator ')'
      {
        $$ = $2;
      }
    | direct_declarator '[' ']'
      {
        $$ = new Object();
        $$.node_type = "unsized_array_dcl";
        $$.dcl = $1;
      }
    | direct_declarator '[' type_qualifier_list ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_declarator3";
        $$.data1 = $1;
        $$.data2 = $3;
      }
    | direct_declarator '[' assignment_expression ']'
      {
        $$ = new Object();
        $$.node_type = "expr_sized_array_dcl";
        $$.size_expr = $3;
        $$.dcl = $1;
      }
    | direct_declarator '[' type_qualifier_list assignment_expression ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_declarator5";
        $$.data1 = $1;
        $$.data2 = $3;
        $$.data3 = $4;
      }
    | direct_declarator '[' STATIC assignment_expression ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_declarator6";
        $$.data1 = $1;
        $$.data2 = $4;
      }
    | direct_declarator '[' STATIC type_qualifier_list assignment_expression ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_declarator7";
        $$.data1 = $1;
        $$.data2 = $4;
        $$.data3 = $5;
      }
    | direct_declarator '[' type_qualifier_list STATIC assignment_expression ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_declarator8";
        $$.data1 = $1;
        $$.data2 = $3;
        $$.data3 = $5;
      }
    | direct_declarator '[' '*' ']'
      {
        $$ = new Object();
        $$.node_type = "unsized_array_dcl";
        $$.dcl = $1;
      }
    | direct_declarator '(' parameter_type_list ')'
      {
        $$ = new Object();
        $$.node_type = "function_dcl";
        $$.dcl = $1;
        $$.params = $3;
      }
    | direct_declarator '(' ')'
      {
        $$ = new Object();
        $$.node_type = "function_dcl";
        $$.dcl = $1;
        $$.params = [];
      }
    | direct_declarator '(' identifier_list ')'
      {
        $$ = new Object();
        $$.node_type = "id_function_dcl";
        $$.dcl = $1;
        $$.param_ids = $3;
      }
    ;

pointer
    : '*' { $$ = ['*']; }
    | '*' type_qualifier_list { $$ = ['*'].concat($2); }
    | '*' pointer { $$ = ['*'].concat($2); }
    | '*' type_qualifier_list pointer { $$ = ['*'].concat($2, $3); }
    ;

type_qualifier_list
    : type_qualifier { $$ = [$1]; }
    | type_qualifier_list type_qualifier
      {
        $1.push($2);
        $$ = $1;
      }
    ;

parameter_type_list
    : parameter_list
    | parameter_list ',' ELLIPSIS
      {
        $$ = new Object();
        $$.node_type = "variadic_params";
        $$.params = $1;
      }
    ;

parameter_list
    : parameter_declaration { $$ = [$1]; }
    | parameter_list ',' parameter_declaration
      {
        $1.push($3);
        $$ = $1;
      }
    ;

parameter_declaration
    : declaration_specifiers declarator
      {
        $$ = new Object();
        $$.node_type = "param";
        $$.type = $1;
        $$.name = $2;
        if(typeof $$.name.node_type !== "undefined") {
          if($$.name.node_type !== "pointer_declarator") {
            throw { message: "Unsupported declarator syntax while generating AST. (1)" };
          }
          $$.type.pointer = $2.pointer;
          $$.name = $2.direct_decl;
          if(typeof $$.name !== "string") {
            throw { message: "Unsupported declarator syntax while generating AST. (2)" };
          }
        }
      }
    | declaration_specifiers
      {
        $$ = new Object();
        $$.node_type = "TODOspec_param_dcl";
        $$.dcl_specs = $1;
      }
    | declaration_specifiers abstract_declarator
      {
        $$ = new Object();
        $$.node_type = "TODO parameter_declaration3";
        $$.data1 = $1;
        $$.data2 = $2;
      }
    ;

identifier_list
    : IDENTIFIER { $$ = [$1]; }
    | identifier_list ',' IDENTIFIER
      {
        $1.push($3);
        $$ = $1;
      }
    ;

type_name
    : specifier_qualifier_list
      {
        $$ = new Object();
        $$.node_type = "TODO specifier_qualifier_list1";
        $$.data1 = $1;
      }
    | specifier_qualifier_list abstract_declarator
      {
        $$ = new Object();
        $$.node_type = "TODO specifier_qualifier_list2";
        $$.data1 = $1;
        $$.data2 = $2;
      }
    ;

abstract_declarator
    : pointer
      {
        $$ = new Object();
        $$.node_type = "TODO abstract_declarator1";
        $$.data1 = $1;
      }
    | direct_abstract_declarator
      {
        $$ = new Object();
        $$.node_type = "TODO abstract_declarator2";
        $$.data1 = $1;
      }
    | pointer direct_abstract_declarator
      {
        $$ = new Object();
        $$.node_type = "TODO abstract_declarator3";
        $$.data1 = $1;
        $$.data2 = $2;
      }
    ;

direct_abstract_declarator
    : '(' abstract_declarator ')'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator1";
        $$.data1 = $2;
      }
    | '[' ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator2";
      }
    | '[' constant_expression ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator3";
        $$.data1 = $2;
      }
    | direct_abstract_declarator '[' ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator4";
        $$.data1 = $1;
      }
    | direct_abstract_declarator '[' assignment_expression ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator5";
        $$.data1 = $1;
        $$.data2 = $3;
      }
    | direct_abstract_declarator '[' '*' ']'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator6";
        $$.data1 = $1;
      }
    | '(' ')'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator7";
      }
    | '(' parameter_type_list ')'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator8";
        $$.data1 = $2;
      }
    | direct_abstract_declarator '(' ')'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator9";
        $$.data1 = $1;
      }
    | direct_abstract_declarator '(' paramter_type_list ')'
      {
        $$ = new Object();
        $$.node_type = "TODO direct_abstract_declarator10";
        $$.data1 = $1;
        $$.data2 = $3;
      }
    ;

typedef_name
    : IDENTIFIER
    ;

initializer
    : assignment_expression
    | '{' initializer_list '}'
      {
        $$ = new Object();
        $$.node_type = "TODO initializer2";
        $$.data1 = $2;
      }
    | '{' initializer_list ',' '}'
      {
        $$ = new Object();
        $$.node_type = "TODO initializer3";
        $$.data1 = $2;
      }
    ;

initializer_list
    : initializer
      {
        $$ = new Object();
        $$.node_type = "TODO initializer_list1";
        $$.data1 = $1;
      }
    | designation initializer
      {
        $$ = new Object();
        $$.node_type = "TODO initializer_list2";
        $$.data1 = $1;
        $$.data2 = $2;
      }
    | initializer_list ',' initializer
      {
        $$ = new Object();
        $$.node_type = "TODO initializer_list3";
        $$.data1 = $1;
        $$.data2 = $3;
      }
    | initializer_list ',' designation initializer
      {
        $$ = new Object();
        $$.node_type = "TODO initializer_list4";
        $$.data1 = $1;
        $$.data2 = $3;
        $$.data3 = $4;
      }
    ;

designation
    : designator_list '='
      {
        $$ = new Object();
        $$.node_type = "TODO designation";
        $$.data1 = $1;
      }
    ;

designator_list
    : designator { $$ = $1; }
    | designator-list designator
      {
        $1.push($2);
        $$ = $1;
      }
    ;

designator
    : '[' constant_expression ']'
      {
        $$ = new Object();
        $$.node_type = "TODO designator1";
        $$.data1 = $2;
      }
    | '.' IDENTIFIER
      {
        $$ = new Object();
        $$.node_type = "TODO designator2";
        $$.data2 = $2;
      }
    ;

/* A.2.3 Statements */

statement
    : labeled_statement
    | compound_statement
    | expression_statement
    | selection_statement
    | iteration_statement
    | jump_statement
    ;

labeled_statement
    : IDENTIFIER ':' statement
      {
        $$ = new Object();
        $$.node_type = "labeled_statement";
        $$.label = $1;
        $$.statement = $3;
      }
    | CASE constant_expression ':' statement
      {
        $$ = new Object();
        $$.node_type = "case";
        $$.guard = $2;
        $$.body = $4;
      }
    | DEFAULT ':' statement
      {
        $$ = new Object();
        $$.node_type = "default";
        $$.body = $3;
      }
    ;

compound_statement
    : '{' '}'
      {
        $$ = new Object();
        $$.node_type = "block";
        $$.contents = [];
      }
    | '{' block_item_list '}'
      {
        $$ = new Object();
        $$.node_type = "block";
        if(typeof $2.node_type !== "undefined") { throw { message: "bad block item list"}; }
        $$.contents = $2;
      }
    ;

block_item_list
    : block_item
      {
        if(typeof $1.node_type === "undefined") {
          $$ = $1;
          if($$[0].node_type !== "declaration") throw {message: "huh" };
        }
        else {
          $$ = new Array(); $$.push($1);
        }
      }
    | block_item_list block_item
      {
        if(typeof ($2.node_type) === "undefined") {
          $1 = $1.concat($2);
        } else {
          $1.push($2);
        }
        $$ = $1;
      }
    ;

block_item
    : declaration
    | statement
    ;

expression_statement
    : ';'
      {
        $$ = new Object();
        $$.node_type = "expression";
        $$.seqs = [];
      }
    | expression ';'
    ;

statement_list
    : statement { $$ = [$1]; }
    | statement_list statement
      {
        $1.push($2);
        $$ = $1;
      }
    ;

selection_statement
    : IF '(' expression ')' statement
      {
        $$ = new Object();
        $$.node_type = "if";
        $$.cond = $3;
        $$.then = $5;
        $$.else = null;
      }
    | IF '(' expression ')' statement ELSE statement
      {
        $$ = new Object();
        $$.node_type = "if";
        $$.cond = $3;
        $$.then = $5;
        $$.else = $7;
      }
    | SWITCH '(' expression ')' statement
      {
        $$ = new Object();
        $$.node_type = "switch";
        $$.param = $3;
        $$.body = $5;
      }
    ;

iteration_statement
    : WHILE '(' expression ')' statement
      {
        $$ = new Object();
        $$.node_type = "while";
        $$.cond = $3;
        $$.body = $5;
      }
    | DO statement WHILE '(' expression ')' ';'
      {
        $$ = new Object();
        $$.node_type = "do_while";
        $$.cond = $5;
        $$.body = $2;
      }
    | FOR '(' expression_statement expression_statement ')' statement
      {
        $$ = new Object();
        $$.node_type = "for";
        $$.init = $3;
        $$.cond = $4;
        $$.action = null;
        $$.body = $6;
      }
    | FOR '(' expression_statement expression_statement expression ')' statement
      {
        $$ = new Object();
        $$.node_type = "for";
        $$.init = $3;
        $$.cond = $4;
        $$.action = $5;
        $$.body = $7;
      }
    | FOR '(' declaration expression_statement ')' statement
      {
        $$ = new Object();
        $$.node_type = "block";
        $$.contents = new Array();
        $$.contents.push(new Object());
        $$.contents[0] = new Object();
        $$.contents[0].node_type = "for";
        $$.contents[0].init = null;
        $$.contents[0].cond = $4;
        $$.contents[0].action = null;
        $$.contents[0].body = $6;
        $$.contents = $3.concat($$.contents); // ahem.
      }
    | FOR '(' declaration expression_statement expression')' statement
      {
        $$ = new Object();
        $$.node_type = "block";
        $$.contents = new Array();
        $$.contents.push(new Object());
        $$.contents[0] = new Object();
        $$.contents[0].node_type = "for";
        $$.contents[0].init = null;
        $$.contents[0].cond = $4;
        $$.contents[0].action = $5;
        $$.contents[0].body = $7;
        $$.contents = $3.concat($$.contents); // ahem.
      }
    ;

jump_statement
    : GOTO IDENTIFIER
      {
        $$ = new Object();
        $$.node_type = "goto";
        $$.target = $2;
      }
    | CONTINUE ';'
      {
        $$ = new Object();
        $$.node_type = "continue";
      }
    | BREAK ';'
      {
        $$ = new Object();
        $$.node_type = "break";
      }
    | RETURN ';'
      {
        $$ = new Object();
        $$.node_type = "return";
        $$.target = null;
      }
    | RETURN expression ';'
      {
        $$ = new Object();
        $$.node_type = "return";
        $$.target = $2;
      }
    ;

/* A.2.4 External definitions */

translation_unit
    : external_declaration
      {
        $$ = new Object();
        $$.node_type = "root"
        if($1.hasOwnProperty("length")) {
          $$.globals = $1;
        } else {
          $$.globals = [$1];
        }
      }
    | translation_unit external_declaration
      {
        if($2.hasOwnProperty("length")) {
          $1.globals = $1.globals.concat($2);
        } else {
          $1.globals.push($2);
        }
        $$ = $1;
      }
    ;

external_declaration
    : function_definition
    | declaration
    ;

function_definition
    : declaration_specifiers declarator compound_statement
      {
        $$ = new Object();
        $$.node_type = "function_definition";
        $$.type = new Object();
        $$.type.node_type = "type";
        $$.type.return_type = $1;
        if($2.node_type === "pointer_declarator") {
          $$.type.return_type.pointer = $2.pointer; // TODO: could return_type.pointer be defined already?
          $2 = $2.direct_decl;
        }
        if($2.node_type !== "function_dcl") {
          throw { message: "Unsupported function definition syntax while generating AST. (1)" };
        }
        if(typeof $2.dcl !== "string") {
          throw { message: "Unsupported function definition syntax while generating AST. (2)" };
        }
        $$.name = $2.dcl;
        $$.type.params = $2.params;
        $$.body = $3;
      }
    | declaration_specifiers declarator declaration_list compound_statement
      {
        $$ = new Object();
        $$.node_type = "TODO function_definition";
        $$.return_type = $1;
        $$.sig = $2;
        $$.body = $4;
        $$.data1 = $3;
      }
    ;

declaration_list
    : declaration { $$ = [$1]; }
    | declaration_list declaration
      {
        $1.push($2);
        $$ = $1;
      }
    ;

start
    : translation_unit EOF { return $$ = $1; }
    ;

