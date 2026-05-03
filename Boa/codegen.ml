(* ARM code generation for the Boa language *)

open Format
open Arm7
open Ast


type env = (string, int) Hashtbl.t  (* Maps variable names to stack offsets *)
let branchCount = 0

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
      Arm7.mov r0 "#" ^ string_of_int (int_of_char c)
    |Cstring s ->
      Arm7.mov r0 "#" ^ string_of_int (int_of_char (List.nth (String.to_list s) 0)) (* We only take the first character from a string and treat it as a char*)
    |Cint i ->
      Arm7.mov r0 "#" ^ string_of_int (Int32.to_int i)
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
    match id with
    |"InputLeft" ->
      Arm7.includeExternal "*Input Left Code*" (*NB: Must put 0 or 1 into r0 *)
    |"InputRight" ->
      Arm7.includeExternal "*Input Right Code*" (*NB: Must put 0 or 1 into r0 *)
    |"InputUp" ->
      Arm7.includeExternal "*Input Up Code*" (*NB: Must put 0 or 1 into r0 *)
    |"InputDown" ->
      Arm7.includeExternal "*Input Down Code*" (*NB: Must put 0 or 1 into r0 *)
    |"InputA" ->
      Arm7.includeExternal "*Input A Code*" (*NB: Must put 0 or 1 into r0 *)
    |"InputB" ->
      Arm7.includeExternal "*Input B Code*" (*NB: Must put 0 or 1 into r0 *)
    |"MoveLeft" ->
      Arm7.includeExternal "*Move Left Code*"
    |"MoveRight" ->
      Arm7.includeExternal "*Move Right Code*" 
    |"MoveUp" ->
      Arm7.includeExternal "*Move Up Code*"
    |"MoveDown" ->  
      Arm7.includeExternal "*Move Down Code*"
    |"Draw" ->
      let compile_expr env (List.nth l 0) in
      (* Now our char to draw is in r0 *)
      Arm7.includeExternal "*Draw Code*"
    

  | Egrid (expr1, expr2) ->
    (* If we had dynamic grids it would be something like this:
    let _ = compile_expr env expr1 in
    Arm7.mov r1 r0
    let _ = compile_expr env expr2 in
    ".include gridDynamic" 

    But we only have support for 3 x 3 grids, so we just import that one: *)
    Arm7.includeExternal "*Arm grid code*"

in
comprec StrMap.empty 0

(* Instruction compilation *)
let compile_instr env (stmt : Ast.stmt) =
  match stmt with
  |Seval expr ->
    let _ = compile_expr env expr in
  |Sif (expr, stmt1, stmt2) ->
    let _ = compile_expr env expr in
    Arm7.cmps r0 "#1" (* "1" should be stored in r0 if the expr is true *)
    (* We make labels for each branch (true and false) *)
    let branchTrue = "Branch" ^ string_of_int branchCount in
    Arm7.branchCC "eq" branchTrue
    branchCount++
    let branchFalse = "Branch" ^ string_of_int branchCount in
    Arm7.branchCC "ne" branchFalse
    branchCount++

    (* We create the labels and put their statements inside *)
    Arm7.newLabel branchTrue
    let _ = compile_instr env stmt1 in

    Arm7.newLabel branchFalse
    let _ = compile_instr env stmt2 in
    (* The arm code of the if-statement should look like this: 
      expr
      cmps r0, #1
      beq BranchX
      bne BranchY
      BranchX:
      stmt1
      BranchY:
      stmt2      
      *)
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
  |Swhile (expr, stmt) ->
    let _ = compile_expr env expr in
    Arm7.cmps r0 "#1" (* "1" should be stored in r0 if the expr is true *)
    let branchTrue = "Branch" ^ string_of_int branchCount in
    Arm7.branchCC "eq" branchTrue
    branchCount++

    Arm7.newLabel branchTrue
    let _ compile_instr env stmt in
    (* Check the expr condition again, and loop if true *)
    let _ compile_expr env expr in
    Arm7.cmps r0 "#1"
    Arm7.branchCC "eq" branchTrue
    (* The arm code of the while-loop should look like:
    expr
    cmps r0, #1
    beq branchX
    branchX:
    stmt
    expr
    cmps r0, #1
    beq branchX
    *)
  |def ({id}, idList, stmt) ->
    nop
    

(* We need a function with the following syntax:
 let codegen_file (ast) output_file = .....
 This function is called in main.ml.
 Use the function "open_out output_file" to create the outputfile and open it. *)