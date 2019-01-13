(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type typ = Int | Bool | Void

type bind = typ * string

type typ = Int | Bool

type bind = typ * string

type expr =
    Literal of int
  | BoolLit of bool
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : bind list;
    body : stmt list;
  }

<<<<<<< HEAD
type program = bind list * func_decl list

(* Pretty-printing functions *)

=======
(*
 * This is declaring a type "program" that is
 * a tuple (i.e. (value1, value2)) of a
 * (list of strings, list of func_decls)
 *)
type program = bind list * func_decl list

>>>>>>> f851ed0f177d3cc593b4812d555eea9cf1ebe6b8
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

<<<<<<< HEAD
let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
=======
(*
 * end of the line
 * finally pattern matching and giving back the string as appropriate
 *)
let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | BoolLIt(true) -> "true"
  | BoolLIt(false) -> "false"
  | Id(s) -> s
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
>>>>>>> f851ed0f177d3cc593b4812d555eea9cf1ebe6b8
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
<<<<<<< HEAD
  | Void -> "void"

let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
=======

(*
 * in microc the only variable declarations are for ints
 * we are adding the human niceties like the spacing, the semicolon
 * and the carriage return
 *)
let string_of_vdecl id = string_of_typ t ^ " " ^ id ^ ";\n"

(*
 * Big payoff: essentially pull out the members of the fdecl (c-struct like)
 * object. spit out the function declaration like one normally sees it 
 * something lik this
 * 
 * function_name(var a, var b) 
 * { 
 * var c; 
 * print(c);
 * }
 *
 * With some helper functions string_of_vdecl and string_of_stmt this is done
 *
 * String.concat s1 strlst
 * will concatentate the items in the strlst inserting s1 inbetween
 * In this case we are sending it "" so that string_of_vdecl really takes over
 *
 * List.map will run the same function over the items in the list
 * [(string_of_vdecl fdecl.local_0) ; ... ; (string_of_vdecl fdecl.locals_n)]
 *)
let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^ ")\n
  {\n" ^
>>>>>>> f851ed0f177d3cc593b4812d555eea9cf1ebe6b8
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
