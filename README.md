# P4 GBA Compiler 

This is our 4th semester project: a small DSL (domain specific language) called **`.gex`**.  
The point of the language is to make it easier to write **3x3 grid games (like Tic-Tac-Toe)** and then **compile it into ARM assembly** that can be used for **GBA (Game Boy Advance)**.

So basically: you write game logic in `.gex`, our compiler parses it + builds an AST, and then codegens `.asm`, which can then be assembled and run on a GBA emulator (like mGBA).

---

## What you need installed

To compile the compiler you need:

- **OCaml** 
- **ocamllex** 
- **Menhir**

---

## How to compile the compiler (build steps)

From the repo root, go into the `Gex` folder and run the same steps we used:

```bash
cd Gex
ocamlc -c ast.ml
ocamllex lexer.mll
menhir --infer --explain parser.mly
ocamlc -o compiler.exe ast.ml parser.mli parser.ml lexer.ml gba_stdlib.ml arm7.ml codegen.ml main.ml
```

If everything worked you should now have:

- `compiler.exe` (our compiler executable)

---

## How to use the compiler (compile .gex files to .asm)

We included a few `.gex` programs with example logic.

### 1) Normal tictactoe (no win conditions)
```bash
./compiler.exe tic_tac_toe.gex output.asm
```

### 2) More complex tictactoe (win conditions + win text)
```bash
./compiler.exe tic_tac_toe_V2.gex output.asm
```

### 3) Spells out “we are gex” in the 3 by 3 grid
```bash
./compiler.exe we_are_gex.gex output.asm
```

### 4) Inverted version of the simple tictactoe
```bash
./compiler.exe simple_inverted.gex output.asm
```

After running any of these you should get `output.asm` generated.

---

## Notes / small details
- The project is focused on **3x3 grid style programs**.
