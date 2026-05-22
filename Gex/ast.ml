(* Abstract Syntax of Gex *)

(* Parsed trees.
   This is the output of the parser and the input of the interpreter. *)

type location = Lexing.position * Lexing.position

type ident = { loc: location; id: string; }

type unop =
  | Uneg (* -e *)  (* NB: We did not implement this in backend *)
  | Unot (* not e *) (* NB: We did not implement this in backend *)

type binop = (* NB: We did not implement Bdiv, Bmod, Band and Bor in backend *)
  | Badd | Bsub | Bmul | Bdiv | Bmod    (* + - * // % *)
  | Beq | Bneq | Blt | Ble | Bgt | Bge  (* == != < <= > >= *)
  | Band | Bor                          (* and or *)

type constant =
  | Cnone
  | Cbool of bool
  | Cchar of char
  | Cstring of string
  | Cint of int32

type expr =
  | Ecst of constant
  | Eident of ident
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr (* NB: We did not implement this in backend *)
  | Ecall of ident * expr list
  | Elist of expr list (* [e1,e2,...] *) (* NB: We did not implement this in backend *)
  | Eget of expr * expr (* e1[e2] *) (* NB: We did not implement this in backend *)
  | Eget2 of expr * expr * expr (* NB: We did not implement this in backend *)
  | Egrid of expr * expr (* changed to two expr by J *)

and stmt =
  | Sif of expr * stmt * stmt
  | Sreturn of expr (* NB: We did not implement this in backend *)
  | Sassign of ident * expr
  | Sprint of expr (* NB: We did not implement this in backend *)
  | Sblock of stmt list
  | Sfor of ident * expr * stmt (* NB: We did not implement this in backend *)
  | Seval of expr
  | Sset of expr * expr * expr (* e1[e2] = e3 *) (* NB: We did not implement this in backend *)
  | Sset2 of expr * expr * expr * expr (* added by DB for a[r,c] = val on grids *) (* NB: We did not implement this in backend *)
  | Swhile of expr * stmt (*added While *)


and def = ident * ident list * stmt

and file_item =
  | Def of (ident * ident list * stmt)
  | Stmt of stmt

and file = def list * stmt