
/* ===================================================== */
/* ============== CS4212 Compiler Design ============== *)
(*        Parsing of MOOL programs into  Asts            *)
(* ===================================================== *)

/* ============ Error reporting Code ============ */
%{

  open Printf
  open MOOL_structs

  let get_pos x = 
    Parsing.rhs_start_pos x
    
   let cnt = ref 0 
   let fresh_label () = (cnt:=!cnt+1; !cnt)
   
   let report_error pos s =
   print_string ("\nFile \"" ^ pos.Lexing.pos_fname ^ "\", line " ^ 
        (string_of_int pos.Lexing.pos_lnum) ^", col "^
        (string_of_int (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))^ ": "
        ^ s ^ "\n"); flush stdout;
  failwith "Error detected"
  
%}

/* =========== Definitions of tokens =========== */

%token ASSIGN
%token INT_KWORD 
%token BOOL_KWORD STRING_KWORD 
%token VOID_KWORD

%token TRUE_KWORD FALSE_KWORD
%token CLASS_KWORD 

%token WHILE_KWORD 
%token IF_KWORD ELSE_KWORD
%token RETURN_KWORD
%token THIS_KWORD 
%token NULL_KWORD
%token NEW_KWORD
%token MAIN_KWORD
%token READ_KWORD
%token PRINT_KWORD
%token EXTENDS_KWORD
%token PRIVATE_KWORD
%token SUPER_KWORD

%token <int> INTEGER_LITERAL
%token <string> STRING_LITERAL

%token <string> VAR_IDENTIFIER
%token <string> CLASS_IDENTIFIER
 
%token NEWLINE

%token OPAREN CPAREN 
%token OBRACE CBRACE

%token SEMICOLON
%token DOT
%token COMMA

%token OR_CONDITION
%token AND_CONDITION
%token LESS
%token GREATER
%token LESS_OR_EQUAL
%token GREATER_OR_EQUAL
%token EQUAL
%token NOT_EQUAL
%token NOT_CONDITION
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE

%token COMMENT_LINE 
%token COMMENT_OPEN COMMENT_CLOSE 

%token EOF

/* ============= Return type of parser ========== */
%start input
%type <MOOL_structs.mOOL_program> input
%% 

/* ========= Grammar rules and actions ========== */

input:    
    class_main  class_decl_list { ($1, $2) }

class_main:
    CLASS_KWORD  CLASS_IDENTIFIER 
        OBRACE method_main CBRACE { ( $2, $4) }
;

class_decl:
    CLASS_KWORD CLASS_IDENTIFIER
        OBRACE 
            var_decl_list  
            method_decl_list
        CBRACE { ($2, None, $4, $5) }
    | CLASS_KWORD CLASS_IDENTIFIER EXTENDS_KWORD CLASS_IDENTIFIER
        OBRACE
            var_decl_list  
            method_decl_list
        CBRACE { ($2, Some $4, $6, $7) }
;

class_decl_list: 
    { [] }
    | non_zero_class_decl_list { List.rev $1 }
;


non_zero_class_decl_list: 
    class_decl                              { [$1] }
    | non_zero_class_decl_list class_decl   { $2 :: $1}
;

method_main:
        VOID_KWORD MAIN_KWORD
        OPAREN mthd_param_list CPAREN 
        OBRACE 
        var_decl_stmt_list 
        non_zero_stmt_list 
        CBRACE 
            { { 
                modifier = Public;
                rettype = VoidT; 
                mOOLid = SimpleVarId "main"; 
                ir3id = (SimpleVarId "main"); 
                params = $4; 
                localvars = $7; stmts = $8;
            }}
;

var_id_rule:
    VAR_IDENTIFIER {  SimpleVarId $1 }
;

method_decl:
        modifier type_KWORD  var_id_rule
        OPAREN mthd_param_list CPAREN 
        OBRACE 
        var_decl_stmt_list 
        non_zero_stmt_list 
        CBRACE 
            { { 
                modifier=$1;
                rettype=$2; 
                mOOLid=$3;
                ir3id= $3;              
                params=$5; 
                localvars=$8;
                stmts=$9;
            }}
; 

/* === Rule for defining the list of field declarations in a class and the first method declaration ===*/

var_decl_list:
    { [] }
    | var_decl_list modifier type_KWORD var_id_rule SEMICOLON { $1 @ [($2, ($3, $4))] }
;


method_decl_list:
    { [] }
    | non_zero_method_decl_list { List.rev $1 }
;

non_zero_method_decl_list:
    method_decl { [$1] }
    | non_zero_method_decl_list method_decl { $2 :: $1 }
;

modifier:
    /* Empty */     { Public }
    | PRIVATE_KWORD { Private }

type_KWORD: 
    BOOL_KWORD      { BoolT }
    | INT_KWORD     { IntT }
    | STRING_KWORD  { StringT }
    | VOID_KWORD    { VoidT }
    | CLASS_IDENTIFIER {  ObjectT $1 }
;


/* === Rule for defining the list of parameters of a method === */
mthd_param_list:
    { [] }
    | non_zero_mthd_param_list { List.rev $1 }
;

non_zero_mthd_param_list: 
    type_KWORD var_id_rule                                  
        { [($1, $2)] }
    | non_zero_mthd_param_list COMMA type_KWORD var_id_rule 
        { ($3, $4) :: $1}
;       


/* === Rules for defining the list of variable declarations in the body of a method === */
var_decl_stmt_list:
    { [] }
    | non_zero_var_decl_stmt_list { List.rev $1 }
;

non_zero_var_decl_stmt_list: 
    type_KWORD var_id_rule  SEMICOLON   { [($1, $2)] }
    | non_zero_var_decl_stmt_list type_KWORD var_id_rule SEMICOLON 
        { ($2, $3) :: $1}
;


/* === Rule for defining the different types of statements in a method body ===*/
stmt: 
    RETURN_KWORD exp SEMICOLON              { ReturnStmt $2 }
    | RETURN_KWORD SEMICOLON                { ReturnVoidStmt}
    | IF_KWORD OPAREN exp CPAREN 
        OBRACE non_zero_stmt_list CBRACE 
        ELSE_KWORD OBRACE non_zero_stmt_list CBRACE     { IfStmt ($3,$6,$10) } 
    | WHILE_KWORD OPAREN exp CPAREN 
        OBRACE non_zero_stmt_list CBRACE    { WhileStmt ($3,$6) } 
    | var_id_rule ASSIGN exp SEMICOLON  { AssignStmt ($1, $3) }
    | READ_KWORD 
        OPAREN var_id_rule CPAREN SEMICOLON     { ReadStmt ($3) }
    | PRINT_KWORD 
        OPAREN exp CPAREN SEMICOLON         { PrintStmt ($3) }
    | atom DOT var_id_rule ASSIGN exp SEMICOLON 
            { AssignFieldStmt ( FieldAccess ( $1, $3), $5) }
    | atom OPAREN exp_list CPAREN SEMICOLON { MdCallStmt (MdCall ( $1, $3)) }
;
/*
stmt_list: 
    { [] }
    | non_zero_stmt_list { $1 }
;
*/

non_zero_stmt_list: 
    stmt                        { [$1] }
    | stmt non_zero_stmt_list   { $1 :: $2}
;


/* === Rule for defining the different types of expressions in a method body ===*/

exp:
    bexp        { $1 }
    | aexp      { $1 }
    | sexp      { $1 }
;

bexp:
    bexp OR_CONDITION conj      { BinaryExp (BooleanOp ("||"), $1, $3) }
    | conj                      { $1 }
;

conj:
    conj AND_CONDITION rexp     { BinaryExp (BooleanOp ("&&"), $1, $3) }
    | rexp                      { $1 }
;

rexp:
    aexp bop aexp   { BinaryExp($2, $1, $3) }
    | bgrd          { $1 }
;

bop:
    LESS                { RelationalOp ("<") }
    | GREATER           { RelationalOp (">") }
    | LESS_OR_EQUAL     { RelationalOp ("<=") }
    | GREATER_OR_EQUAL  { RelationalOp (">=") }
    | EQUAL             { RelationalOp ("==") }
    | NOT_EQUAL         { RelationalOp ("!=") }
;

bgrd:
    NOT_CONDITION bgrd      { UnaryExp (UnaryOp ("!"), $2) }
    | TRUE_KWORD            { BoolLiteral (true) }
    | FALSE_KWORD           { BoolLiteral (false) }
    | atom                  { $1 }
;

aexp:
    aexp PLUS term          { BinaryExp (AritmeticOp ("+"), $1, $3) }
    | aexp MINUS term       { BinaryExp (AritmeticOp ("-"), $1, $3) }
    | term                  { $1 }
;

term:
    term MULTIPLY ftr       { BinaryExp (AritmeticOp ("*"), $1, $3) }
    | term DIVIDE ftr       { BinaryExp (AritmeticOp ("/"), $1, $3) }
    | ftr                   { $1 }
;

ftr:
    INTEGER_LITERAL         { IntLiteral ( $1 ) }
    | MINUS ftr             { UnaryExp (AritmeticOp ("-"), $2) }
    | atom                  { $1 }
;

sexp:
    STRING_LITERAL          { StringLiteral ( $1 ) }
    | atom                  { $1 }
;

atom:
    atom DOT var_id_rule                        { FieldAccess ( $1, $3) }
    | THIS_KWORD                                { ThisWord }
    | NULL_KWORD                                { NullWord }
    | SUPER_KWORD                               { SuperWord }
    | var_id_rule                               { Var $1 }
    | OPAREN exp CPAREN                         { $2 }
    | OPAREN CLASS_IDENTIFIER CPAREN atom       { CastExp ($4, ObjectT $2) }
    | NEW_KWORD CLASS_IDENTIFIER OPAREN CPAREN  { ObjectCreate ($2) }
    | atom OPAREN exp_list CPAREN               { MdCall ($1, $3) }
;

exp_list: 
    { [] }
    | non_zero_exp_list { List.rev $1 }
;

non_zero_exp_list: 
    exp                         { [$1] }
    | non_zero_exp_list COMMA exp       { $3 :: $1}
;
    
%%