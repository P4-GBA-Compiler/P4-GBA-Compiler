(* ARM code generation for the Boa language *)

open Format
open Arm7
open Ast


(* Compiling expressions. *)
let compile_expr =
  (* Recursive function compile_expr used to generate ARM code of the
     abstract syntax tree associated with a value of type Ast.expr;
     at the end of the execution of this code, the translation of value must be
     placed at the top of the stack *)
  let rec comprec (env : int StrMap.t) (next : int) (e : Ast.expr) : Arm7.text =
    match e with
    | ECst i ->
      match i with
      | Cnone
      | Cbool b ->
        Arm7.mov "r0" (if b then 1 else 0)
      |Cchar c ->
        Arm7.mov "r0" int(char(c))
      |Cstring s ->
        nop
      |Cint i ->
        Arm7.mov "r0" i
    | EVar x ->
      nop (* TODO 1 *)
    | EBinop (o,e1,e2)->
      nop (* TODO 1 *)
  in
  comprec StrMap.empty 0

(* Instruction compilation *)
let compile_instr (i : Ast.stmt): Arm7.text =
  match i with
  | Set (x, e) ->
    nop (* TODO 2 *)
  | Print e ->
    nop (* TODO 2 *)


(* We need a function with the following syntax:
 let codegen_file (ast) output_file = .....
 This function is called in main.ml.
 Use the function "open_out output_file" to create the outputfile and open it. *)