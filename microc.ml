type action = Ast | Interpret | Bytecode | Compile

(*
 * Complete entry point for the running compiler
 * 
 * First let _ means don't hold onto the value returned
 * from the following procedures
 * 
 * if Sys.argv is 0 then by default the action is
 * Ast else returns action so that Action is used in
 * "match action with" line.
 *
 * List.assoc will take the argument from Sys.argv
 * which is an array dereferenced by (1)
 * the key values tuples are stored in an ocaml list
 * when the appropriate item is found the value
 * of the tuple is returned to be assigned to
 * action
 *)
let _ =
  let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
			      ("-i", Interpret);
			      ("-b", Bytecode);
			      ("-c", Compile) ]
  else Compile in
  (*
   * Now action is set either by the if or the else.
   * else will assign Action to Compile and then the
   * following code will use the variable named
   * action
   *
   * Lexing.from_channel
   * Lexing is the run-time library for lexers (aka scanners)
   * generated by ocamllex. from_channel will create a lexer
   * buffer on a given input channel. stdin is the the predefined
   * unix input channel.
   *
   * Scanner.token
   * refers to the token member function created in the
   * scanner.mll document sending lexbuf in as the argument
   * to the token function.
   *)
  
  let lexbuf = Lexing.from_channel stdin in
  (*
   * program is a tuple representing (vars, funcs)
   * vars = a list of the global variables declared
   * funcs = a list of the functions declared/defined
   *         and all the information that goes with it
   *)
  let program = Parser.program Scanner.token lexbuf in
  match action with
    (*
     * the ast flag is simply a way of reviewing the input program
     * after being scanned and parsed. We revisit the global variables
     * parsed and the functions delcared. We print them all out
     * as we understand them now that they are parsed.
     *)
    Ast -> let listing = Ast.string_of_program program
           in print_string listing
    (*
     * the ignore funciton will allow its argument to execute
     * and return unit = (). This is necessary to keep the 
     * return types of let _ = consistent. An OCaml requirement
     * 
     * We now head over to the Interpret module to ue the run
     * member function
     *)
  | Interpret -> ignore (Interpret.run program)
  | Bytecode -> let listing =
      Bytecode.string_of_prog (Compile.translate program)
    in print_endline listing
  | Compile -> Execute.execute_prog (Compile.translate program)
 
