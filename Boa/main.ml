open Lexing

(* To use the compiler you input the following on the command line:
ocamlc -o compiler ast.ml lexer.ml parser.ml codegen.ml main.ml
compiler.exe input.boa output.asm
./compiler input.boa output.asm (for mac)
"compiler" is just the name of the file, it can be anything. *)


(* This function is essentially the "main" that run when program starts.
  "()" mean that it takes no arguments *)
let () =
  (* "Sys.argv" is an array of command line arguments. 
    The first input "Sys.argv.(0)" is the program name. This can be anything.
    The second input "Sys.argv.(1)" is the input file.
    The last (optional) input "Sys.argv.(2)" is the output file, which will default to
    "output.asm" if unspecified. *)
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <input.boa> <output.asm>\n" Sys.argv.(0);
    exit 1
  end;
  
  (* Here we save the input and output files from the command line arguments. *)
  let input_file = Sys.argv.(1) in
  let output_file = if Array.length Sys.argv > 2 then Sys.argv.(2) else "output.asm" in
  
  try
    (* Here we open the input file to be read from.
      "open_in" opens the file for reading and returns an input channel.
      "Lexing.from_channel" wraps the channel into a lexbuffer so we can
      read a stream of characters.*)
    let input_read = open_in input_file in
    let lexbuf = Lexing.from_channel input_read in
    
    (* Parser.file = parser.mly 
       Lexer = lexer.mll
       The parser repeatedly calls the "next_token" function from lexer.mll
       to generate the ast. *)
    let ast = Parser.file Lexer.next_token lexbuf in
    close_in input_read; (* When the ast is made, we can close the input file. *)
    
    (* Codegen = codegen.ml
      We call the "codegen_file" function with the parameters "ast" and "output_file",
      which matches the function definition *)
    Codegen.codegen_file ast output_file;
    
    (* Now the file has been compiled to assembly *)
    Printf.printf "Compilation successful: %s\n" output_file
    
  with
   (* Here we use the Lexing_error function to catch invalid characters *)
  | Lexer.Lexing_error msg -> 
      Printf.eprintf "Lexical error: %s\n" msg; exit 1
   (* Here we use the regular error function to catch invalid characters *)
  | Parser.Error ->
      Printf.eprintf "Parse error at %s\n" 
        (string_of_int lexbuf.lex_curr_p.pos_cnum);
      exit 1