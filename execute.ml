open Ast
open Bytecode

let execute_prog prog =
  (*
   * OCaml Reminder:
   * Array.make 1024 0 will create an array of 1024 zeros
   *)
  let stack = Array.make 1024 0
  and globals = Array.make prog.num_globals 0 in

  let rec exec fp sp pc = match prog.text.(pc) with
    (*
     * Example run of the above prog
     * step 3)
     * Lit 42 -> stack.(3) <- 42 ; exec 1 (3+1) (3+1)
     *)
    Lit i  -> stack.(sp) <- i ; exec fp (sp+1) (pc+1)
  | Drp    -> exec fp (sp-1) (pc+1)
  | Bin op -> let op1 = stack.(sp-2) and op2 = stack.(sp-1) in     
      stack.(sp-2) <- (
        let boolean i = if i then 1 else 0 in
        let int2bool i = if (i=1) then true else if (i=0) then false else true in
      match op with
	    | Add     -> op1 + op2
      | Sub     -> op1 - op2
      | Mult    -> op1 * op2
      | Div     -> op1 / op2
      | And     -> boolean (int2bool(op1) && int2bool(op2))
      | Or      -> boolean (int2bool(op1) || int2bool(op2))
      | Equal   -> boolean (op1 =  op2)
      | Neq     -> boolean (op1 != op2)
      | Less    -> boolean (op1 <  op2)
      | Leq     -> boolean (op1 <= op2)
      | Greater -> boolean (op1 >  op2)
      | Geq     -> boolean (op1 >= op2)) ;
      exec fp (sp-1) (pc+1)
  (*
   * Ocaml Reminder:
   * Arrays are mutable and stack.(sp) <- stack.(fp+i) is setting stack.(sp)
   * with stack.(fp+i)
   *)
  | Lod i   -> stack.(sp)   <- globals.(i)  ; exec fp (sp+1) (pc+1)
  | Str i   -> globals.(i)  <- stack.(sp-1) ; exec fp sp     (pc+1)
  | Lfp i   -> stack.(sp)   <- stack.(fp+i) ; exec fp (sp+1) (pc+1)
  (*
   * Example run of the above prog
   * step 4)
   * Sfp 1 -> stack.(2) <- stack.(4-1) ; exec 1 4 (4+1)
   *)
  | Sfp i   -> stack.(fp+i) <- stack.(sp-1) ; exec fp sp     (pc+1)
  | Jsr(-1) -> print_endline (string_of_int stack.(sp-1)) ; exec fp sp (pc+1)
  (*
   * Example run of the above prog
   * step 1)
   * Jsr 2 -> stack.(0) <- 0+1 ; exec 0 (0+1) 2
   *
   * Unconditional jump to instruction 2
   * Store on the top of the stack program counter + 1 (next source code)
   * next: move onto the next instruction
   * keep the frame pointer the same
   * iterate the stack pointer
   * change the program counter to the instruction we're currently on
   *)
  | Jsr i   -> stack.(sp)   <- pc + 1       ; exec fp (sp+1) i
  (*
   * Example run of the above prog
   * step 2)
   * Ent 1 -> stack.(1) <- 0 ; exec 1 (1+1+1) 3
   *
   * Store the old frame pointer where the stack pointer currently is (just past the
   * end of the last activation record).
   * Next: move onto the next instruction (Jsr 2)+1
   * execute the next instruction with the current SP as the new FP
   * and with the current SP+sizeOfStackSpaceWeneed+OnePastEnd
   *)
  | Ent i   -> stack.(sp)   <- fp           ; exec sp (sp+i+1) (pc+1)
  | Rts i   -> let new_fp = stack.(fp) and new_pc = stack.(fp-1) in
               stack.(fp-i-1) <- stack.(sp-1) ; exec new_fp (fp-i) new_pc
  | Beq i   -> exec fp (sp-1) (pc + if stack.(sp-1) =  0 then i else 1)
  | Bne i   -> exec fp (sp-1) (pc + if stack.(sp-1) != 0 then i else 1)
  | Bra i   -> exec fp sp (pc+i)
  | Hlt     -> ()

  in exec 0 0 0
