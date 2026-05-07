(* ARM code generation for the Boa language *)

open Format
open Arm7
open Ast

(* Global counter for variables. Is necessary to keep stack offsets
   from overlapping between main environment and function environment *)
let varCount = ref 1
(* Global counter for branch-labels *)
let branchCount = ref 0

(* Global table to store function definitions for later reference *)
let functions : (string, int list) Hashtbl.t = Hashtbl.create 10

(* Compiling function calls *)
let rec compile_function_call env fName args =
  let offSets = Hashtbl.find functions fName in
  List.iteri (fun i arg ->
    let _ = compile_expr env arg in
    Arm7.push r0 (List.nth offSets i)
    ) args;

  Arm7.branchLink fName;

(* Compiling function definitions *)
let compile_function_def (func : Ast.def) =
  let (funcIdent, args, _) = func in
  let funcName = funcIdent.id in
  let argOffsets = ref [] in

  (* Map arguments to stack offsets so they can be referenced later *)
  List.iter (fun arg ->
    let offset = !varCount * 4 in
    incr varCount;
    argOffsets := !argOffsets @ [offset]
    )args;
  
  (* We map the function name to it's list of variable offsets to the global functions hashtable *)
  Hashtbl.add functions funcName !argOffsets;

(* Compile function bodies separately *)
let compile_function_body env (func : Ast.def) =
  let (funcIdent, args, funcBody) = func in
  let funcName = funcIdent.id in
  let argOffsets = Hashtbl.find functions funcName in

  (* Generate branch label. So assembly prints "funcName:" *)
  Arm7.newLabel funcName;

  (* Save the current state of the environment so we can revert it to this state later. *)
  let oldEnv = Hashtbl.copy env in  

  (* We add the local variables from the function's arguments to the environment we pass to the compilation of the body.
     We now have access to both global variables declared in main, but also the ones declared in the function definition. *)
  List.iteri (fun i arg ->
    Hashtbl.add env arg (List.nth argOffsets i)
    ) args;

  (* Compile the function body with updated environment*)
  let _ = compile_instr env funcBody in
  
  (* After compiling the body, we now remove the function arguments from the main environment. *)
  List.iteri (fun arg i ->
    Hashtbl.remove env arg
    ) args;
  
  (* Any variables created within the function body must be removed from the environment so future functions do not have
     access to these variables. We do this by comparing it with the old environment state before the function body was compiled. *)
  Hashtbl.iter (fun var _ ->
    if not(Hashtbl.mem oldEnv var) then
      Hashtbl.remove env var
    ) env;

  (* Reset the oldEnv environment to save space, as we will no longer use it. *)
  Hashtbl.reset oldEnv;

  (* End function by pushing lr back into the pc to return the after the function was called *)
  Arm7.mov pc lr;

(* Compiling expressions. *)
  (* Recursive function compile_expr used to generate ARM code of the
     abstract syntax tree associated with a value of type Ast.expr;
     at the end of the execution of this code, the translation of value must be
     placed at the top of the stack *)
let rec compile_expr env (expr : Ast.expr) =
  match expr with
  | Ecst constant ->
    match constant with
    | Cnone ->
      Arm7.mov r0 "#0" (* Default value *)
    | Cbool b ->
      Arm7.mov r0 (if b then "#1" else "#0")
    |Cchar c ->
      Arm7.mov r0 ("#" ^ string_of_int (int_of_char c))
    |Cstring s ->
      Arm7.mov r0 ("#" ^ string_of_int (int_of_char (s.[0]))) (* We only take the first character from a string and treat it as a char*)
    |Cint i ->
      Arm7.mov r0 ("#" ^ string_of_int (Int32.to_int i))
  | Eident {id} ->
    if not (Hashtbl.mem env id) then error "unbound variable";
    let stackOffset = Hashtbl.find env id in
    Arm7.pop r0 stackOffset
  | Ebinop (operand, expr1, expr2)->
    let _ = compile_expr env expr1 in
    Arm7.mov r1 r0;
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
  | Ecall ({id}, args)->
    match id.id with
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
      let _ = compile_expr env (List.nth args 0) in
      (* Now our char to draw is in r0 *)
      Arm7.includeExternal "*Draw Code*"
    | _ ->
      (* User-defined function call *)
      if not (Hashtbl.mem functions id.id) then
        failwith ("Undefined Function: " ^ id.id)
      else if List.length args != List.length(Hashtbl.find functions id) then
        failwith ("Incorrect amount of arguments passed to function: " ^ id)
      else
        compile_function_call env id args (* We pass only the global environment if we call a function *)

  | Egrid (expr1, expr2) ->
    (* If we had dynamic grids it would be something like this:
    let _ = compile_expr env expr1 in
    Arm7.mov r1 r0
    let _ = compile_expr env expr2 in
    ".include gridDynamic" 

    But we only have support for 3 x 3 grids, so we just import that one: *)
    Arm7.includeExternal "*Arm 3 x 3 grid code*";

(* Instruction compilation *)
and compile_instr env (stmt : Ast.stmt) =
  match stmt with
  | Seval expr ->
    let _ = compile_expr env expr in
  | Sif (expr, stmt1, stmt2) ->
    let _ = compile_expr env expr in
    Arm7.cmps r0 "#1"; (* "1" should be stored in r0 if the expr is true *)
    (* We make labels for each branch (true and false) *)
    let branchTrue = ("Branch" ^ string_of_int !branchCount) in
    Arm7.branchCC "eq" branchTrue;
    incr branchCount;
    let branchFalse = ("Branch" ^ string_of_int !branchCount) in
    Arm7.branchCC "ne" branchFalse;
    incr branchCount;

    (* We create the labels and put their statements inside *)
    Arm7.newLabel branchTrue;
    let _ = compile_instr env stmt1 in

    Arm7.newLabel branchFalse;
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
    let _ = compile_expr env expr in (* This stores the "expr" in r0 *)
    (* If the variable is already assigned, we find it in the env.
      If not, then we add a new offset which extends the stack frame by 4 bytes. *)
    let offset = if Hashtbl.mem env id then Hashtbl.find env id else
      let offNew = !varCount * 4 in
      incr varCount;
      Hashtbl.add env id offNew
      offNew (* This value is stored in "offset" if the variable did not exist prior *)
    in
    Arm7.push r0 offset
  |Sblock block ->
    List.iter (compile_instr env) block
  |Swhile (expr, stmt) ->
    let _ = compile_expr env expr in
    Arm7.cmps r0 "#1"; (* "1" should be stored in r0 if the expr is true *)
    let branchTrue = ("Branch" ^ string_of_int !branchCount) in
    Arm7.branchCC "eq" branchTrue;
    incr branchCount;

    Arm7.newLabel branchTrue;
    let _ = compile_instr env stmt in
    (* Check the expr condition again, and loop if true *)
    let _ = compile_expr env expr in
    Arm7.cmps r0 "#1";
    Arm7.branchCC "eq" branchTrue;
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
    
(* We need a function with the following syntax:
 let codegen_file (defs, main_stmt) output_file = .....
 This function is called in main.ml.
 Use the function "open_out output_file" to create the outputfile and open it. *)
 let codegen_file ((defs, main_stmt) : Ast.file) output_file =
  (* Initialise environment. *)
  let main_env : (string, int) Hashtbl.t = Hashtbl.create 20 in
  (* Initialise frame pointer to point at a certain position on the stack. *)
  Arm7.add fp sp "#8";

  (* Compile order:
     Function def (with input parameters): *)
  List.iter (fun def -> compile_function_def def) defs;
  (* Main statements (includes function calls): *)
  let _ = compile_instr main_env main_stmt in
  (* Function bodies (they can now access global variables created by the main statements): *)
  List.iter (fun def -> compile_function_body main_env def) defs;