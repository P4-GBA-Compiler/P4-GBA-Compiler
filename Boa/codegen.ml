(* ARM code generation for the Boa language *)

open Format
open Arm7
open Ast

(* Compiling expressions. *)
let compile_expr =
  (* Recursive function compile_expr used to generate MIPS code of the
     abstract syntax tree associated with a value of type Ast.expr;
     at the end of the execution of this code, the translation of value must be
     placed at the top of the pile *)
  let rec comprec (env : int StrMap.t) (next : int) (e : Ast.expr) : Arm7.text =
    match e with
    | ECst i ->
      match i with
      | Cnone
      | Cbool b ->
        nop
      |Cchar c ->
        nop
      |Cstring s ->
        nop
      |Cint i ->
        nop
    | EVar x ->
      nop (* TODO 1 *)
    | EBinop (o,e1,e2)->
      nop (* TODO 1 *)
  in
  comprec StrMap.empty 0

(* Instruction compilation *)
let compile_instr (i : Ast.stmt): Mips.text =
  match i with
  | Set (x, e) ->
    nop (* TODO 2 *)
  | Print e ->
    nop (* TODO 2 *)


(* We need a function with the following syntax:
 let codegen_file (ast) output_file = .....
 This function is called in main.ml.
 Use the function "open_out output_file" to create the outputfile and open it. *)