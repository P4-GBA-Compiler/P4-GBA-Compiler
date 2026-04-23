open Lexing

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <input.boa> <output.asm>\n" Sys.argv.(0);
    exit 1
  end;
  
  let input_file = Sys.argv.(1) in
  let output_file = if Array.length Sys.argv > 2 then Sys.argv.(2) else "output.asm" in
  
  try
    let ic = open_in input_file in
    let lexbuf = Lexing.from_channel ic in
    
    (* Parse *)
    let ast = Parser.file Lexer.next_token lexbuf in
    close_in ic;
    
    (* Generate code *)
    Codegen.codegen_file ast output_file;
    
    Printf.printf "Compilation successful: %s\n" output_file
    
  with
  | Lexer.Lexing_error msg -> 
      Printf.eprintf "Lexical error: %s\n" msg; exit 1
  | Parser.Error ->
      Printf.eprintf "Parse error at %s\n" 
        (string_of_int lexbuf.lex_curr_p.pos_cnum);
      exit 1