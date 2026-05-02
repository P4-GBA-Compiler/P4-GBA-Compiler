(* ARM code generation for the Boa language *)

open Format
open Arm7
open Ast


type env = (string, int) Hashtbl.t  (* Maps variable names to stack offsets *)
type benv = (string, )

(* Compiling expressions. *)
  (* Recursive function compile_expr used to generate ARM code of the
     abstract syntax tree associated with a value of type Ast.expr;
     at the end of the execution of this code, the translation of value must be
     placed at the top of the stack *)
let rec compile_expr env (expr : Ast.expr) =
  match expr with
  | ECst constant ->
    match constant with
    | Cnone -> nop
    | Cbool b ->
      Arm7.mov r0 (if b then "#1" else "#0")
    |Cchar c ->
      Arm7.mov r0 Cat("#", string_of_int (int_of_char c))
    |Cstring s ->
      Arm7.mov r0 Cat("#", string_of_int (int_of_char (String.sub s 0 1))) (* We only take the first character from a string and treat it as a char*)
    |Cint i ->
      Arm7.mov r0 Cat("#", string_of_int i)
  | Eident {id} ->
    if not (Hashtbl.mem env id) then error "unbound variable";
    let stackOffset = Hashtbl.find env id in
    Arm7.pop r0 stackOffset
  | Ebinop (operand, expr1, expr2)->
    let _ = compile_expr env expr1 in
    Arm7.mov r1 r0
    let _ = compile_expr env expr2 in
    match operand with
    | Badd ->
      Arm7.add r0 r1 r0
    | Bsub ->
      Arm7.sub r0 r1 r0
    | Beq ->
      Arm7.cmps r1 r0 (* Sets Z flag if values are equal *)
      Arm7.movCC "eq" r0 1 (* CC = eq makes instruction happen if Z flag is set *)
      Arm7.movCC "ne" r0 0 (* CC = ne makes instruction happen if Z flag is not set *)
  | Ecall ({id}, l)->
    nop
  | Egrid l (* maybe change to two Cint? *) ->
    nop
in
comprec StrMap.empty 0

(* Instruction compilation *)
let compile_instr env (stmt : Ast.stmt) =
  match stmt with
  |Sif (expr, stmt1, stmt2) ->
    let _ = compile_expr env expr in
    Arm7.cmps r0 "#1"
    Arm7.branchCC "eq" "name?" (* Maybe use a global list of branches and update it every time we make an if/for/while stmt? *)

    let _ = compile_instr env stmt1 in
    let _ = compile_instr env stmt2 in
  | Sassign ({id}, expr) ->
    let _ = compile_expr env expr in (* This stores the "e" in r0 *)
    (* If the variable is already assigned, we find it in the env.
      If not, then we add a new offset which extends the stack frame by 4 bytes. *)
    let offset = if Hashtbl.mem env id then Hashtbl.find env id else
      let offNew = (Hashtbl.length env) * 4 in
      Hashtbl.add env offNew
      offNew (* This value is stored in "offset" if the variable did not exist prior *)
    in
    Arm7.push r0 offset
  |Sblock block ->
    List.iter (compile_instr env) block
  |Sfor ({id}, expr, stmt) ->
    nop
  |Swhile (expr, stmt) ->
    nop
  |def ({id}, idList, stmt) ->
    nop
    

(* We need a function with the following syntax:
 let codegen_file (ast) output_file = .....
 This function is called in main.ml.
 Use the function "open_out output_file" to create the outputfile and open it. *)