open Ast
open Gba_stdlib

(* In OCaml, variables are immutable by default. However, when walking through
   an Abstract Syntax Tree (AST), we sometimes need to remember what we have 
   seen so far. We use a 'type' record with "mutable" fields to keep track of this. *)
type emit_context = {
  out: out_channel;                (* Where we write the generated assembly text *)
  mutable saw_while: bool;         (* Tracks whether we encountered a while loop *)
  mutable emitted_loop_exit: bool; (* Tracks whether we already printed loop exit + switch *)
}

(* This function writes our raw assembly strings into the output file. 
   It safely checks if the string already ends with a newline ('\n'). 
   If it doesn't, it adds one so the next assembly instruction doesn't break. *)
let emit ctx text =
  output_string ctx.out text;                         (* Append raw template text to file *)
  let len = String.length text in                     (* Get the length of the string *)
  if len = 0 || text.[len - 1] <> '\n' then           (* If the last character is NOT a newline... *)
    output_char ctx.out '\n'                          (* ...add a newline character safely. *)

(* These small functions look at pieces of the AST and answer True/False.
   "Ecall" stands for Expression Call (like a function call).
   "Ecst" stands for Expression Constant (like a string or number). *)

(* Checks if an expression is a function call matching a specific name. *)
let is_call name = function
  | Ecall (id, _) -> id.id = name  (* Direct function call with matching identifier *)
  | _ -> false                     (* Not a call expression, so it's not a match *)

(* Checks if an AST node is exactly "Turn("x")" or "Turn("o")". *)
let is_turn_call expected = function
  | Seval (Ecall (id, [Ecst (Cstring value)])) -> id.id = "Turn" && value = expected (* Direct Turn call *)
  | Sblock [Seval (Ecall (id, [Ecst (Cstring value)]))] -> id.id = "Turn" && value = expected (* Single-stmt block *)
  | _ -> false                                                                            (* Anything else is not Turn *)

(* Checks if the AST node is testing 'symbol == "x"'.
   "Ebinop" stands for Expression Binary Operator (like +, -, or ==). *)
let is_symbol_eq_x = function
  | Ebinop (Beq, Eident id, Ecst (Cstring "x")) -> id.id = "symbol"   (* symbol == "x" *)
  | Ebinop (Beq, Ecst (Cstring "x"), Eident id) -> id.id = "symbol"   (* "x" == symbol *)
  | _ -> false                                                        (* Not a match, return false *)

(* Detects the specific if/else block used to switch turns at the end of the loop. *)
let is_turn_switch_if cond then_s else_s =
  is_symbol_eq_x cond && is_turn_call "o" then_s && is_turn_call "x" else_s  (* if symbol == "x" then Turn("o") else Turn("x") *)

(* Here we translate AST conditions (like "InputLeft()") into GBA Assembly tests. 
   We use "match" to see which input function was called and emit the right assembly. *)
let compile_condition ctx expr =
  match expr with
  | Ecall (id, _) -> begin                               (* Simple input function call condition *)
      match id.id with
      | "InputLeft" -> emit ctx cond_if_left            (* Print: LEFT button test template *)
      | "InputRight" -> emit ctx cond_if_right          (* Print: RIGHT button test template *)
      | "InputUp" -> emit ctx cond_if_up                (* Print: UP button test template *)
      | "InputDown" -> emit ctx cond_if_down            (* Print: DOWN button test template *)
      | "InputA" -> emit ctx cond_if_a                  (* Print: A button test template *)
      | _ -> failwith "Unsupported input condition"     (* Stop compiler if unknown input *)
    end
  | _ -> failwith "Unsupported condition in if/elif chain"    (* Any other condition is out of MVI scope *)

(* This is the main recursive function that reads the AST instructions ("S" nodes)
   and prints the corresponding Assembly templates. It uses "match" to figure out 
   what type of statement it is looking at. *)
let rec compile_stmt ctx ~in_loop stmt =
  match stmt with
  | Sblock stmts -> List.iter (compile_stmt ctx ~in_loop) stmts  (* Compile each statement in order *)
  | Sassign (id, expr) ->                                        (* Handle assignments used by the MVI *)
      if id.id = "selecting" then begin                         (* Special case: selecting = False breaks loop *)
        match expr with
        | Ecst (Cbool false) -> emit ctx cond_break               (* selecting = False -> Breaks the loop *)
        | _ -> ()                                                 (* Ignore other selecting assignments *)
      end else begin
        match expr with
        | Egrid _ -> emit ctx global_setup                        (* grid a = (3,3) => emit global setup calls *)
        | _ -> ()                                                 (* Ignore other assignments in MVI *)
      end

  | Swhile (_cond, body) ->                                      (* Compile the single while loop *)
      ctx.saw_while <- true;                                     (* Remember that we generated a loop *)
      emit ctx while_start;                                      (* Print: TurnLoop: and read joystick *)
      compile_stmt ctx ~in_loop:true body                        (* Recursively compile everything inside the loop *)
      
  | Sif (cond, then_s, else_s) ->                                 (* Compile if/elif chains or special turn switch *)
      if is_turn_switch_if cond then_s else_s then begin                      (* Detect the turn-switching if/else *)
        emit ctx label_exit;                                     (* Print: TurnLoopExit: *)
        emit ctx switch_turn;                                    (* Print turn switching assembly *)
        ctx.emitted_loop_exit <- true                            (* Record that we printed loop exit sequence *)
      end else if in_loop then begin
        compile_if_chain ctx cond then_s else_s                  (* Emit if/elif templates inside the loop *)
      end else begin
        ()                                                       (* Ignore other if-statements outside loop *)
      end

  | Seval (Ecall (id, _args)) -> begin                            (* Compile action calls like MoveLeft/Draw *)
      match id.id with
      | "MoveLeft" -> emit ctx act_move_left                     (* Print: bl MoveLeft *)
      | "MoveRight" -> emit ctx act_move_right                   (* Print: bl MoveRight *)
      | "MoveUp" -> emit ctx act_move_up                         (* Print: bl MoveUp *)
      | "MoveDown" -> emit ctx act_move_down                     (* Print: bl MoveDown *)
      | "Draw" ->                                                
          emit ctx act_draw;                                     (* Print: bl TryPlaceSymbol *)
          emit ctx act_wait_release                              (* Print: bl WaitForReleaseAny *)
      | "Turn" -> ()                                             (* Ignore Turn(...) calls in MVI *)
      | _ -> ()                                                   (* Ignore other calls in MVI *)
    end
  | Seval _ -> ()                                                 (* Ignore other expressions in MVI *)
  | _ -> ()                                                       (* Ignore unsupported statements in MVI *)

(* This function handles the sequence of button presses. If a button is NOT pressed, 
   it branches forward to the next "1:" label (bne 1f). If it IS pressed,
   it runs the action, and then jumps back to the top of the while loop. *)
and compile_if_chain ctx cond then_s else_s =
  compile_condition ctx cond;                                  (* 1. Print condition test template with bne 1f *)
  compile_stmt ctx ~in_loop:true then_s;                       (* 2. Print true-branch action templates (e.g., MoveLeft)*)
  emit ctx jump_loop;                                          (* 3. Each successful branch restarts the loop *)
  emit ctx label_next_cond;                                    (* 4. Place 1: label for the next 'elif' to catch *)
  match else_s with
  | Sif (next_cond, next_then, next_else) ->                    (* Next elif becomes another chain step *)
    compile_if_chain ctx next_cond next_then next_else        (* Recurse to compile the next condition *)
  | Sblock [] -> emit ctx jump_loop                             (* Final fallthrough: jump back to loop *)
  | _ ->
    compile_stmt ctx ~in_loop:true else_s;                    (* If there is a final 'else', compile it *)
    emit ctx jump_loop                                       (* And jump back to TurnLoop when done  *)

(* In this MVI, we ignore the function wrapper "def Turn(symbol):" and 
   just extract and compile the code block inside of it directly. *)
let compile_def ctx (name, _args, body) =
  if name.id = "Turn" then compile_stmt ctx ~in_loop:false body (* Ignore all other function defs *)

(* This function dictates the exact order the final ".asm" file is written.
   It is important that libraries go at the very bottom, otherwise the GBA
   CPU will try to execute them as if they are the main game code! *)
let compile_to_file (defs, body) output_file =
  (* "open_out" opens the file for writing. *)
  let oc = open_out output_file in                              (* Create output assembly file *)
  let ctx = { out = oc; saw_while = false; emitted_loop_exit = false } in (* Initialize emitter state *)

  (* Setup & Header *)
  emit ctx grid_config;                                         (* Print WRAM and constant definitions *)
  emit ctx gba_header;                                          (* Print GBA boot header and ProgramStart label *)

  (* Main Program Logic *)
  compile_stmt ctx ~in_loop:false body;                         (* Compile top-level statements first *)
  List.iter (compile_def ctx) defs;                             (* Then compile the Turn definition body *)

  (* Loop Exit Fallback *)
  if ctx.saw_while && not ctx.emitted_loop_exit then begin      (* Ensure loop exit exists if not already emitted *)
    emit ctx label_exit;                                        (* Print TurnLoopExit label *)
    emit ctx switch_turn                                        (* Print SwitchTurn and loop restart *)
  end;

  (* Hardware Libraries (Must be at the bottom) *)
  emit ctx grid_funcs;                                          (* Append grid setup functions at file end *)
  emit ctx movement;                                            (* Append movement libraries *)
  emit ctx drawing;                                             (* Append drawing libraries *)
  emit ctx gba_graphics;                                        (* Append graphics libraries *)
  emit ctx gba_hardware;                                        (* Append hardware libraries *)
  close_out oc                                                  (* Close output file *)
