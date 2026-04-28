open Format

type register =  string
let r0 : register = "r0"
let r1 : register = "r1"
let r2 : register = "r2"
let r3 : register = "r3"
let r4 : register = "r4"
let r5 : register = "r5"
let r6 : register = "r6"
let r7 : register = "r7"
let r8 : register = "r8"
let r9 : register = "r9"
let r10 : register = "r10"
let r11 : register = "r11"
let r12 : register = "r12"
let sp : register = "sp"
let lr : register = "lr"
let pc : register = "pc"

(* Leon's code: *)
type label = string
type 'a address = formatter -> 'a -> unit
let alab : label address = fun fmt  (s : label) -> fprintf fmt "%s" s
let areg : (int * register) address = fun fmt (x, y) -> fprintf fmt "%i(%s)" x y
type 'a operand = formatter -> 'a -> unit
let oreg : register operand = fun fmt (r : register) -> fprintf fmt "%s" r
let oi : int operand = fun fmt i -> fprintf fmt "%i" i
let oi32 : int32 operand = fun fmt i -> fprintf fmt "%li" i

type 'a asm =
  | Nop
  | S of string
  | Cat of 'a asm * 'a asm

let buf = Buffer.create 17
let fmt = formatter_of_buffer buf
let ins x =
  Buffer.add_char buf '\t';
  kfprintf (fun fmt ->
    fprintf fmt "\n";
    pp_print_flush fmt ();
    let s = Buffer.contents buf in
    Buffer.clear buf;
    S s
  ) fmt x

let pr_list fmt pr = function
  | []      -> ()
  | [i]     -> pr fmt i
  | i :: ll -> pr fmt i; List.iter (fun i -> fprintf fmt ", %a" pr i) ll

let pr_ilist fmt l =
  pr_list fmt (fun fmt i -> fprintf fmt "%i" i) l

let pr_alist fmt l =
  pr_list fmt (fun fmt (a : label) -> fprintf fmt "%s" a) l

(* End of Leon's code *)

let add a b c = ins "add %s, %s, #%a" a b c
let mov a b = ins "mov %s, #%a" a b

let (++) x y = Cat (x, y)

let mov dest operand = ins "move %s, %s" dest operand