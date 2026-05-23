(* ARM code generation for the Gex language *)

open Format
open Arm7
open Ast
open Gba_stdlib

(* Global counter for variables. Is necessary to keep stack offsets
   from overlapping between main environment and function environment *)
let varCount = ref 1
(* Global counter for branch-labels *)
let branchCount = ref 0

(* Global table to store function definitions for later reference *)
let functions : (string, int list) Hashtbl.t = Hashtbl.create 10

(* Compiling function definitions (Does not depend on expressions) *)
let compile_function_def (func : Ast.def) =
  let (funcIdent, args, _) = func in
  let funcName = funcIdent.id in
  let argOffsets = ref [] in

  if Hashtbl.mem functions funcName then failwith ("Function '" ^ funcName ^ "' defined more than once.");
  (* Map arguments to stack offsets so they can be referenced later *)
  List.iter (fun _arg ->
    let offset = !varCount * 4 in
    incr varCount;
    argOffsets := !argOffsets @ [offset]
    ) args;
  
  (* We map the function name to it's list of variable offsets to the global functions hashtable *)
  Hashtbl.add functions funcName !argOffsets

(* Compiling expressions. *)
  (* Recursive function compile_expr used to generate ARM code of the
     abstract syntax tree associated with a value of type Ast.expr;
     at the end of the execution of this code, the translation of value must be
     placed at the top of the stack *)
let rec compile_expr env (expr : Ast.expr) =
  match expr with
  | Ecst constant ->
    begin match constant with
    | Cnone ->
      Arm7.mov r0 "#0" (* Default value *)
    | Cbool b ->
      Arm7.mov r0 (if b then "#1" else "#0")
    | Cstring s ->
      Arm7.mov r0 ("#" ^ string_of_int (int_of_char (s.[0]))) (* We only take the first character from a string and treat it as a char*)
    | Cint i ->
      Arm7.mov r0 ("#" ^ string_of_int (Int32.to_int i))
    end
  | Eident {id} ->
    if not (Hashtbl.mem env id) then failwith "unbound variable";
    let stackOffset = Hashtbl.find env id in
    Arm7.load r0 stackOffset
  | Ebinop (operand, expr1, expr2) ->
    compile_expr env expr1;
    Arm7.mov r1 r0;
    compile_expr env expr2;
    begin match operand with
    | Badd ->
      Arm7.add r0 r1 r0
    | Bsub ->
      Arm7.sub r0 r1 r0
    | Bmul ->
      Arm7.mul r0 r1 r0
    | Beq ->
      Arm7.cmps r1 r0; (* Sets Z flag if values are equal *)
      Arm7.movCC "eq" r0 ("#" ^ string_of_int 1); (* CC = eq makes instruction happen if Z flag is set *)
      Arm7.movCC "ne" r0 ("#" ^ string_of_int 0) (* CC = ne makes instruction happen if Z flag is not set *)
    | Bneq ->
      Arm7.cmps r1 r0; (* Sets Z flag if values are equal *)
      Arm7.movCC "eq" r0 ("#" ^ string_of_int 0); (* CC = eq makes instruction happen if Z flag is set *)
      Arm7.movCC "ne" r0 ("#" ^ string_of_int 1) (* CC = ne makes instruction happen if Z flag is not set *)
    | Blt ->
      Arm7.cmps r1 r0; (* Sets flags *)
      Arm7.mov r0 ("#" ^ string_of_int 0);
      Arm7.movCC "lt" r0 ("#" ^ string_of_int 1) (* CC = lt makes instruction happen if N flag set and V clear, or if N clear and V set *)
    | Ble ->
      Arm7.cmps r1 r0; (* Sets flags *)
      Arm7.mov  r0 ("#" ^ string_of_int 0);
      Arm7.movCC "le" r0 ("#" ^ string_of_int 1) (* CC = le makes instruction happen if Z set or N set and V clear, or N clear and V set *)
    | Bgt ->
      Arm7.cmps r1 r0; (* Sets flags *)
      Arm7.mov r0 ("#" ^ string_of_int 0);
      Arm7.movCC "gt" r0 ("#" ^ string_of_int 1) (* CC = gt makes instruction happen if Z clear and N set or V set or N clear and V clear *)
    | Bge ->
      Arm7.cmps r1 r0; (* Sets flags *)
      Arm7.mov r0 ("#" ^ string_of_int 0);
      Arm7.movCC "ge" r0 ("#" ^ string_of_int 1) (* CC = ge makes instruction happen if N set and V set or N clear and V clear *)
    | _ -> failwith "Unsupported binop"
    end
  | Ecall (func_ident, args) ->
    let func_name = func_ident.id in
    begin match func_name with
    | "InputLeft" ->
      Arm7.branchLink "InputLeft"
    | "InputRight" ->
      Arm7.branchLink "InputRight"
    | "InputUp" ->
      Arm7.branchLink "InputUp"
    | "InputDown" ->
      Arm7.branchLink "InputDown"
    | "InputA" ->
      Arm7.branchLink "InputA"
    | "InputB" ->
      Arm7.branchLink "InputB";
    | "MoveLeft" ->
      Arm7.branchLink "MoveLeft";
      Arm7.branchLink "WaitForReleaseAny"
    | "MoveRight" ->
      Arm7.branchLink "MoveRight";
      Arm7.branchLink "WaitForReleaseAny"
    | "MoveUp" ->
      Arm7.branchLink "MoveUp";
      Arm7.branchLink "WaitForReleaseAny"
    | "MoveDown" ->
      Arm7.branchLink "MoveDown";
      Arm7.branchLink "WaitForReleaseAny"
    | "Draw" ->
      compile_expr env (List.nth args 0);
      Arm7.branchLink "TryPlaceSymbol";
      Arm7.push "r0"; 
      Arm7.branchLink "WaitForReleaseAny";
      Arm7.pop "r0"
    | _ ->
      (* User-defined function call *)
      if not (Hashtbl.mem functions func_name) then
        failwith ("Undefined Function: " ^ func_name)
      else if List.length args <> List.length (Hashtbl.find functions func_name) then
        failwith ("Incorrect amount of arguments passed to function: " ^ func_name)
      else
        compile_function_call env func_name args (* We pass only the global environment if we call a function *)
    end
  | Egrid (_expr1, _expr2) ->
    Arm7.branchLink "ScreenInit";
    Arm7.branchLink "InitGameState";
    Arm7.branchLink "DrawGrid";
    Arm7.branchLink "ShowCursor"
  | _ -> ()

(* Instruction compilation *)
and compile_instr env (stmt : Ast.stmt) =
  match stmt with
  | Seval expr ->
    compile_expr env expr
  | Sif (expr, stmt1, stmt2) ->
    compile_expr env expr;
    Arm7.cmps r0 "#1"; (* "1" should be stored in r0 if the expr is true *)

    (* We make labels for each branch (true and false) *)
    let branchTrue = ("BranchTrue" ^ string_of_int !branchCount) in
    Arm7.branchCC "eq" branchTrue;
    incr branchCount;
    
    let branchFalse = ("BranchFalse" ^ string_of_int !branchCount) in
    Arm7.branchCC "ne" branchFalse;
    incr branchCount;

    let branchEnd = ("BranchEnd" ^ string_of_int !branchCount) in
    incr branchCount;

    (* We create the labels and put their statements inside *)
    Arm7.newLabel branchTrue;
    compile_instr env stmt1;
    Arm7.branchCC "al" branchEnd;
    Arm7.newLabel branchFalse;
    compile_instr env stmt2;

    Arm7.newLabel branchEnd
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
    compile_expr env expr; (* This stores the "expr" in r0 *)
    (* If the variable is already assigned, we find it in the env.
      If not, then we add a new offset which extends the stack frame by 4 bytes. *)
    let offset = 
      if Hashtbl.mem env id then 
        Hashtbl.find env id 
      else begin
      let offNew = !varCount * 4 in
      incr varCount;
      Hashtbl.add env id offNew;
      offNew (* This value is stored in "offset" if the variable did not exist prior *)
      end
    in
    Arm7.store r0 offset
  | Sblock block ->
    List.iter (compile_instr env) block
  | Swhile (expr, stmt) ->
    compile_expr env expr;
    Arm7.cmps r0 "#1"; (* "1" should be stored in r0 if the expr is true *)
    let branchTrue = ("BranchWhileTrue" ^ string_of_int !branchCount) in
    Arm7.branchCC "eq" branchTrue;
    incr branchCount;

    Arm7.newLabel branchTrue;
    compile_instr env stmt;
    (* Check the expr condition again, and loop if true *)
    compile_expr env expr;
    Arm7.cmps r0 "#1";
    Arm7.branchCC "eq" branchTrue
  | _ -> ()

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
 (* Compiling function calls *)
and compile_function_call env fName args =
  let offSets = Hashtbl.find functions fName in
  List.iteri (fun i arg ->
    compile_expr env arg;
    Arm7.store r0 (List.nth offSets i)
    ) args;

  Arm7.branchLink fName   


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
    Hashtbl.add env arg.id (List.nth argOffsets i)
    ) args;

  (* Function prologue - Save link register so nested functions don't clobber it *)
  Arm7.push "lr";

  (* Compile the function body with updated environment*)
  compile_instr env funcBody;
  
  (* After compiling the body, we now remove the function arguments from the main environment using their string ID. *)
  List.iter (fun arg -> 
    Hashtbl.remove env arg.id
    ) args;
  
  (* Any variables created within the function body must be removed from the environment so future functions do not have
     access to these variables. We do this by comparing it with the old environment state before the function body was compiled. *)
  Hashtbl.iter (fun var _ ->
    if not (Hashtbl.mem oldEnv var) then
      Hashtbl.remove env var
    ) env;

  (* Reset the oldEnv environment to save space, as we will no longer use it. *)
  Hashtbl.reset oldEnv;

  (* Function epilogue - Pop the saved link register directly into the program counter *)
  Arm7.pop "pc"

(* We need a function with the following syntax:
 let codegen_file (defs, main_stmt) output_file = .....
 This function is called in main.ml.
 Use the function "open_out output_file" to create the outputfile and open it. *)
let codegen_file ((defs, main_stmt) : Ast.file) output_file =
  let out_channel = open_out output_file in
  output_string out_channel grid_config;
  output_string out_channel gba_header;

  Arm7.newLabel "ProgramStart";

  (* Initialise environment. *)
  let main_env : (string, int) Hashtbl.t = Hashtbl.create 20 in
  (* Initialise frame pointer to point at a certain position on the stack. *)
  Arm7.add fp sp "#8";

  (* Compile order:
    Function def (with input parameters): *)
  List.iter (fun def -> compile_function_def def) defs;
  (* Main statements (includes function calls): *)
  compile_instr main_env main_stmt;
  (* End program by going to "EndProg" loop *)
  Arm7.branch "EndProg";
  (* Function bodies (they can now access global variables created by the main statements): *)
  List.iter (fun def -> compile_function_body main_env def) defs;
  
  (* EndProg loop *)
  Arm7.newLabel "EndProg";
  Arm7.branch "EndProg";

  Arm7.write_to_file out_channel;

  output_string out_channel input_helpers;
  output_string out_channel movement;
  output_string out_channel drawing;
  output_string out_channel gba_graphics;
  output_string out_channel gba_hardware;
  close_out out_channel