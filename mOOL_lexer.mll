(* ===================================================== *)
(* ============== CS4212 Compiler Design ============== *)
(* 			  	 Lexing of MOOL programs 			 	 *)
(* ===================================================== *)

{
  open MOOL_parser (* Assumes the parser file is "parser.mly". *)
  let incr_linenum file_name lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
      lexbuf.Lexing.lex_curr_p <- 
	{ pos with
	    Lexing.pos_fname = file_name;
	    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
	    Lexing.pos_bol = pos.Lexing.pos_cnum;
	}
}

let digit = ['0'-'9']
let varid = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let classid = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let charhex = ['0'-'9' 'A'-'F' 'a'-'f']
let whitespace = [' ' '\t']
let newline = ('\n' | '\r' | "\r\n")
let charprintable = ['\032' - '\126']
let stringliteral = ("\\\"" | "\\\\" | "\\n" | "\\r" | "\\t" | "\\b" 
        | "\\x"['0'-'7'] charhex
        | "\\0" digit digit | "\\1"['0'-'2']['0'-'7'] | [^ '\n' '\r' '\"' '\\'])*
let singlelinecomment = "//"[^ '\n' '\r']*(newline|eof)

rule token file_name = parse
  | '=' 	{ ASSIGN }
  | '{'		{ OBRACE }
  | '}'		{ CBRACE }
  | '('		{ OPAREN }
  | ')'		{ CPAREN }
  | "Int"	{ INT_KWORD}
  | "Bool"	{ BOOL_KWORD}
  | "String" { STRING_KWORD}
  | "true"	{ TRUE_KWORD }
  | "false"	{ FALSE_KWORD }
  | "class" { CLASS_KWORD }
  | "Void"  { VOID_KWORD }
  | "while"	{ WHILE_KWORD }
  | "if" 	{ IF_KWORD }
  | "else"	{ ELSE_KWORD }
  | "return" { RETURN_KWORD }
  | "this"	{ THIS_KWORD }
  | "NULL" { NULL_KWORD }
  | "main"  { MAIN_KWORD }
  | "readln" { READ_KWORD }
  | "println" { PRINT_KWORD }
  | "extends" { EXTENDS_KWORD }
  | "private" { PRIVATE_KWORD }
  | "super" { SUPER_KWORD }
  | "new" { NEW_KWORD }
  | ';'		{ SEMICOLON }
  | '.'		{ DOT }
  | ','		{ COMMA }
  | classid as str		{ CLASS_IDENTIFIER str}
  | varid as str1		{ VAR_IDENTIFIER str1 }
  | digit+ as num
		{ INTEGER_LITERAL (int_of_string num) }
  |  "\""( stringliteral
          as s) 
	"\"" { STRING_LITERAL s }
  | "||" { OR_CONDITION }
  | "&&" { AND_CONDITION }
  | '<' { LESS }
  | '>' { GREATER }
  | "<=" { LESS_OR_EQUAL }
  | ">=" { GREATER_OR_EQUAL }
  | "==" { EQUAL }
  | "!=" { NOT_EQUAL }
  | '!' { NOT_CONDITION }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { MULTIPLY }
  | '/' { DIVIDE }
  | whitespace { token file_name lexbuf }
  | '\n' { incr_linenum file_name lexbuf; token file_name lexbuf }
  | '\r' { incr_linenum file_name lexbuf; token file_name lexbuf }
  | "\r\n" { incr_linenum file_name lexbuf; token file_name lexbuf }
  | singlelinecomment { incr_linenum file_name lexbuf; token file_name lexbuf }
  | "/*" { multi_line_comments 0 file_name lexbuf  }
  | eof  { EOF }
  
and multi_line_comments level file_name = parse 
  | "*/" { if level = 0 then token file_name lexbuf
             else multi_line_comments (level-1) file_name lexbuf
         }
  | "/*" { multi_line_comments (level+1) file_name lexbuf }
  | '\n' { incr_linenum file_name lexbuf; multi_line_comments level file_name lexbuf }
  | '\r' { incr_linenum file_name lexbuf; multi_line_comments level file_name lexbuf }
  | "\r\n" { incr_linenum file_name lexbuf; multi_line_comments level file_name lexbuf }
  | _    { multi_line_comments level file_name lexbuf }
  | eof		{ raise End_of_file }

  
