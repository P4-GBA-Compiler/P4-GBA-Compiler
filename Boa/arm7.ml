open Format (*Delete this later to see if it is not necessary*)

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

type 'a asm =
  | Nop
  | S of string
  | Cat of 'a asm * 'a asm

let (++) x y = Cat (x, y)

let mov dest operand = ins "move %s, %s" dest operand