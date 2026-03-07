# Lyte Grammar (EBNF)

Reference grammar derived from the parser (`src/parser.rs`) and lexer (`src/lexer.rs`).

## Lexical Elements

```ebnf
(* Whitespace and comments *)
WHITESPACE   = " " | "\t" ;
NEWLINE      = "\n" ;
COMMENT      = "//" { any - "\n" } ;

(* Identifiers and literals *)
alpha        = "a".."z" | "A".."Z" | "_" ;
digit        = "0".."9" ;
id           = ( alpha ) { alpha | digit } ;

integer      = digit { digit } ;
uinteger     = integer "u" ;
real         = digit { digit } "." digit { digit } ;
char_lit     = "'" ( char | "\\" ( "\\" | "n" ) ) "'" ;
string_lit   = '"' { any - '"' } '"' ;

(* Keywords *)
keyword      = "if" | "else" | "while" | "for" | "in" | "return"
             | "var" | "let" | "fn" | "struct" | "enum" | "macro"
             | "interface" | "where" | "arena" | "as"
             | "true" | "false"
             | "void" | "bool" | "i8" | "i32" | "u32" | "f32" | "f64" ;

(* Operators and punctuation *)
(* ->  or  U+2192 (→)          arrow
   ..                           range
   ==  !=  <=  >=  <  >        comparison
   ||  &&                       logical
   +  -  *  /  ^  !            arithmetic / unary
   =                            assignment
   |                            lambda param delimiter
   .  :  ,  ;  @               misc punctuation
   ⟨ ⟩                         math-angle typevar delimiters (U+27E8, U+27E9)
   ⋅                            alternate multiply (U+22C5)                    *)
```

## Types

```ebnf
type         = basic_type { "->" basic_type } ;

basic_type   = "void" | "bool"
             | "i8" | "i32" | "u32" | "f32" | "f64"
             | "⟨" id "⟩"
             | "typevar" id
             | "[" type "]"                        (* slice *)
             | "[" type ";" integer "]"            (* fixed-size array *)
             | "[" type ";" id "]"                 (* size-parameterized array *)
             | id [ "<" typelist ">" ]              (* named type, optionally generic *)
             | "(" type ")" ;

typelist     = type { "," type } ;
typevar_list = "<" id { "," id } ">" ;
```

## Expressions

```ebnf
expr         = "if" if_expr
             | assign ;

if_expr      = expr block [ "else" ( "if" if_expr | block ) ] ;

assign       = equality { "=" equality } ;
equality     = logic { ( "==" | "!=" ) logic } ;
logic        = relation { ( "||" | "&&" ) relation } ;
relation     = sum { ( "<" | ">" | "<=" | ">=" ) sum } ;
sum          = term { ( "+" | "-" ) term } ;
term         = power { ( "*" | "/" ) power } ;
power        = factor { "^" factor } ;

factor       = "-" factor
             | "+" factor
             | "!" factor
             | postfix ;

postfix      = atom { "(" [ exprlist ] ")"         (* function call *)
                     | "[" expr "]"                 (* array index *)
                     | "." ( id | integer )         (* field access / tuple index *)
                     | ":" type                     (* type ascription *)
                     | "as" type                    (* type cast *)
                     } ;

atom         = id
             | integer | uinteger | real
             | char_lit | string_lit
             | "true" | "false"
             | "." id                               (* enum variant *)
             | "(" lambda { "," lambda } ")"        (* parenthesized expr or tuple *)
             | block                                (* block expression *)
             | array_literal
             | "@" id "(" exprlist ")" ;            (* macro invocation *)

lambda       = "|" [ paramlist ] "|" lambda
             | expr ;

exprlist     = lambda { "," lambda } ;
array_literal = "[" [ lambda { "," lambda } ] "]"   (* list literal *)
              | "[" lambda ";" expr "]" ;            (* repeat literal *)
```

## Statements

Statements appear inside blocks, separated by newlines.

```ebnf
stmt         = "var" id "=" lambda                  (* mutable variable *)
             | "var" id ":" type                    (* mutable variable with type *)
             | "let" id "=" lambda                  (* immutable binding *)
             | "while" expr block                   (* while loop *)
             | "for" id "in" expr ".." expr block   (* for loop *)
             | "return" expr                        (* return *)
             | "arena" block                        (* arena scope *)
             | expr ;                               (* expression statement *)

block        = "{" { NEWLINE } [ stmt { NEWLINE stmt } ] { NEWLINE } "}" ;
```

## Declarations

A program is a sequence of declarations.

```ebnf
program      = { NEWLINE } { decl { NEWLINE } } ;

decl         = func_decl
             | struct_decl
             | enum_decl
             | global_decl
             | interface_decl
             | macro_decl ;

func_decl    = [ "fn" ] id [ typevar_list ] [ "(" [ paramlist ] ")" ]
               [ "->" type ]
               [ "where" { id typevar_list } ]
               [ block ] ;

macro_decl   = "macro" id [ typevar_list ] [ "(" [ paramlist ] ")" ]
               [ "->" type ] [ block ] ;

struct_decl  = "struct" id [ typevar_list ] "{" [ fieldlist ] "}" ;
enum_decl    = "enum" id "{" [ caselist ] "}" ;
global_decl  = "var" id ":" type ;

interface_decl = "interface" id [ typevar_list ] "{" { func_decl } "}" ;

paramlist    = param { "," param } ;
param        = id [ ":" type ] ;

fieldlist    = field { "," field } ;
field        = id ":" type ;

caselist     = id { "," id } ;
```

## Operator Precedence (low to high)

| Precedence | Operators          | Associativity |
|------------|--------------------|---------------|
| 1          | `=`                | Left          |
| 2          | `\|\|` `&&`        | Left          |
| 3          | `==` `!=`          | Left          |
| 4          | `<` `>` `<=` `>=`  | Left          |
| 5          | `+` `-`            | Left          |
| 6          | `*` `/` `%`        | Left          |
| 7          | `^`                | Left          |
| 8          | `-` `+` `!` (unary)| Right (prefix)|
| 9          | `()` `[]` `.` `:` `as` | Left (postfix) |
