open Ast

(* 函数与变量的符号表 *)
module NameMap = Map.Make(struct 
  type t = string
  let compare x y = Pervasives.compare x y
end )

(* 函数与变量的类型表 *)
module TypeMap = Map.Make(struct
  type t = typ
  let compare x y = Pervasives.compare x y
end )

exception ReturnException of int * int NameMap.t

(* Map 使用： https://ocaml.org/learn/tutorials/map.zh.html
   Map.Make 函子： http://www.ocamljava.org/files/api/stdlib/Map.Make.html
   List 使用： https://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html *)
let run (vars, funcs) = 
  let func_decls = List.fold_left
    (fun fmap fd_n -> NameMap.add fd_n.fname fd_n fmap) NameMap.empty funcs (* fmap： 函数名与函数体映射表*)
  in
  let func_typ_decls = List.fold_left
    (fun ftmap fd_n -> TypeMap.add fd_n.typ fd_n.fname ftmap) TypeMap.empty funcs  (* ftmap： 函数类型与函数名映射表*)
  in
  let rec call fdecl actuals globals = 
    (* 对一个表达式求值并返回 （value, updated environment） *)
    let rec eval env = function
      Literal(i) -> i, env
      | BoolLit(b) -> (if b then 1 else 0), env
      | Noexpr -> 1, env
      | Id(var) -> let locals, globals = env
        in 
          if NameMap.mem var locals then (NameMap.find var locals), env
          else if NameMap.mem var globals then (NameMap.find var globals), env
          else raise (Failure ("undeclared identifier " ^ var))
      | Binop(e1, op, e2) ->
          let v1, env = eval env e1 in
          let v2, env = eval env e2 in
          let boolean i = if i then 0 else 1 in
          let int2bool i = if (i=1) then true else if (i=0) then false else true 
          in
            (match op with
              | Add -> v1 + v2
              | Sub -> v1 - v2
              | Mult -> v1 * v2
              | Div -> v1 / v2
              | And -> boolean (int2bool(v1) && int2bool(v2))
              | Or -> boolean (int2bool(v1) || int2bool(v2))
              | Equal -> boolean (v1 = v2)
              | Neq -> boolean (v1 != v2)
              | Less -> boolean (v1 < v2)
              | Leq -> boolean (v1 <= v2)
              | Greater -> boolean (v1 > v2)
              | Geq -> boolean (v1 >= v2)), env
      | Assign(var, e) -> let v, (locals, globals) = eval env e in
          if NameMap.mem var locals then v, (NameMap.add var v locals, globals)
          else if NameMap.mem var globals then v, (locals, NameMap.add var v globals)
          else raise (Failure ("undeclared identifier " ^ var))
      | Call("print", [e]) -> let v, env = eval env e in print_endline (string_of_int v); 0, env
      | Call(f, actuals) -> let fdecl = try NameMap.find f func_decls with Not_found -> raise (Failure ("undefined function " ^ f))
          in 
            let actuals, env = List.fold_left (fun(actuals, env) actual -> let v, env = eval env actual in v :: actuals, env) ([], env)(List.rev actuals)
            in
              let (locals, globals) = env
        in 
          try
            let globals = call fdecl actuals globals
            in
              0, (locals, globals)
          with ReturnException(v, globals) -> v, (locals, globals)
    in
      (* 带参数的 call, 执行一条语句并返回更新后的环境 *)
      let rec exec env = function
        | Expr(e) -> let _, env = eval env e in env
        | If(e, s1, s2) -> let v, env = eval env e in exec env (if v != 0 then s1 else s2)
        | While(e, s) -> let rec loop env = let v, env = eval env e in if v != 0 then loop (exec env s) else env in loop env
        | For(e1, e2, e3, s) -> let _, env = eval env e1 in let rec loop env = let v, env = eval env e2 in if v != 0 then let _, env = eval (exec env s) e3 in loop env else env in loop env
        | Return(e) -> let v, (locals, globals) = eval env e in raise (ReturnException(v, globals))
      in 
        let arglocals =
          try List.fold_left2
            (fun locals formal actual -> NameMap.add formal actual locals)
            NameMap.empty (List.map snd fdecl.formals) actuals
          with Invalid_argument(_) ->
            raise (Failure ("wrong number of arguments passed to " ^ fdecl.fname))
        in 
        (* todo: type switch with string*)
        let initValueByType ft = 
          (match fst ft with
          | Int -> 0
          | Bool -> 1
          | Void -> 0)
        in
        let locals = List.fold_left
        (fun lmap ld -> NameMap.add (snd ld) (initValueByType ld) lmap) arglocals fdecl.locals
        in
          snd (List.fold_left exec (locals, globals) fdecl.body)
        in
        (* todo: type switch with string*)
        let initValueByType ft = 
          (match fst ft with
          | Int -> 0
          | Bool -> 1
          | Void -> 0)
        in
        let globals = List.fold_left
          (fun gmap vd -> NameMap.add (snd vd) (initValueByType vd) gmap) NameMap.empty vars
        in
          try
            call (NameMap.find "main" func_decls) [] globals
            with Not_found -> raise (Failure ("did not find the main() function"))
