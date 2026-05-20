let grid_config = {|
; TicTacToe screen mode3
; WRAM works by making, each byte address store one game value.
.equ StateBase,0x02000000	; Base address for our WRAM 
.equ GridCellsCount,9		; 9 bytes for 3x3 logical board cells
.equ CursorX_Off,9			; Offset +9: cursor X tile index (0..2)
.equ CursorY_Off,10			; Offset +10: cursor Y tile index (0..2)
.equ CurrentSymbol_Off,11	; Offset +11: whose turn (ASCII byte, eg. 'x','o')

.equ FontScale,5			; Multiply 8x8 font by 5 to get a 40x40 pixel character
.equ FontOffsetX,20			; (80 cell width - 40 char width) / 2 = 20 (Center X)
.equ FontOffsetY,6			; (53 cell height - 40 char height) / 2 = 6 (Center Y)

.equ CellEmpty,0			; Stored value for empty tile
.equ CellX,35				; Stored value for X tile as ASCII 'X'
.equ CellO,79				; Stored value for O tile as ASCII 'O'
|}

let gba_header = {|
	.org  0x08000000     ; GBA ROM Address starts at 0x08000000

; --- Borrowed from Sprite_Moving.asm ---
	b	ProgramStart	;000h    4     ROM Entry Point  (32bit ARM branch opcode, eg. "B rom_start")

;004h    156   Nintendo Logo    (compressed bitmap, required!)
	.byte 0xC8,0x60,0x4F,0xE2,0x01,0x70,0x8F,0xE2,0x17,0xFF,0x2F,0xE1,0x12,0x4F,0x11,0x48     ; C
	.byte 0x12,0x4C,0x20,0x60,0x64,0x60,0x7C,0x62,0x30,0x1C,0x39,0x1C,0x10,0x4A,0x00,0xF0     ; D
    .byte 0x14,0xF8,0x30,0x6A,0x80,0x19,0xB1,0x6A,0xF2,0x6A,0x00,0xF0,0x0B,0xF8,0x30,0x6B     ; E
    .byte 0x80,0x19,0xB1,0x6B,0xF2,0x6B,0x00,0xF0,0x08,0xF8,0x70,0x6A,0x77,0x6B,0x07,0x4C     ; F
    .byte 0x60,0x60,0x38,0x47,0x07,0x4B,0xD2,0x18,0x9A,0x43,0x07,0x4B,0x92,0x08,0xD2,0x18     ; 10
    .byte 0x0C,0xDF,0xF7,0x46,0x04,0xF0,0x1F,0xE5,0x00,0xFE,0x7F,0x02,0xF0,0xFF,0x7F,0x02     ; 11
    .byte 0xF0,0x01,0x00,0x00,0xFF,0x01,0x00,0x00,0x00,0x00,0x00,0x04,0x00,0x00,0x00,0x00     ; 12
    .byte 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00     ; 13
    .byte 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00     ; 14
	.byte 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x1A,0x9E,0x7B,0xEB     ; 15

    ;		123456789012
    .ascii "LEARNASM.NET";0A0h    12    Game Title       (uppercase ascii, max 12 characters)
    .ascii "0000"	;0ACh    4     Game Code        (uppercase ascii, 4 characters)
    .ascii "00"		;0B0h    2     Maker Code       (uppercase ascii, 2 characters)
	.byte 0x96		;0B2h    1     Fixed value      (must be 96h, required!)
	.byte 0			;0B3h    1     Main unit code   (00h for current GBA models)
	.byte 0			;0B4h    1     Device type      (usually 00h) (bit7=DACS/debug related)
	.space 7		;0B5h    7     Reserved Area    (should be zero filled)
	.byte 0			;0BCh    1     Software version (usually 00h)
	.byte 0			;0BDh    1     Complement check (header checksum, required!)
	.word 0			;0BEh    2     Reserved Area    (should be zero filled)
	.long 0			;0C0h    4     RAM Entry Point  (32bit ARM branch opcode, eg. "B ram_start")
	.byte 0			;0C4h    1     Boot mode        (init as 00h - BIOS overwrites this value!)
	.byte 0			;0C5h    1     Slave ID Number  (init as 00h - BIOS overwrites this value!)
	.space 26		;0C6h    26    Not used         (seems to be unused)
	.long 0			;0E0h    4     JOYBUS Entry Pt. (32bit ARM branch opcode, eg. "B joy_start")
; ----------------------------------------------------------
; --- 	 		End of Predefined Libraries v1 		 	 ---
; ----------------------------------------------------------
|}
let global_setup = {|
	bl ScreenInit				; Enter Mode 3 and clear screen
	bl InitGameState			; Clear board memory and set first turn
	bl DrawGrid					; Draw static 3x3 grid lines
	bl ShowCursor				; XOR draw the cursor at starting tile [0,0]
|}

let input_left_call = {|
	bl InputLeft
|}

let input_right_call = {|
	bl InputRight
|}

let input_up_call = {|
	bl InputUp
|}

let input_down_call = {|
	bl InputDown
|}

let input_a_call = {|
	bl InputA
|}

let input_b_call = {|
	bl InputB
|}

let move_left_call = {|
	bl MoveLeft
|}

let move_right_call = {|
	bl MoveRight
|}

let move_up_call = {|
	bl MoveUp
|}

let move_down_call = {|
	bl MoveDown
|}

let draw_call = {|
	bl TryPlaceSymbol
|}

let wait_release_call = {|
	bl WaitForReleaseAny
|}

let input_helpers = {|
; ----------------------------------------------------------
; --- 		Start of Predefined Libraries v2 		 	 ---
; ----------------------------------------------------------
; --- Input helpers (return 1=pressed, 0=released in r0) ---
InputLeft:
	STMFD sp!,{r2-r12,lr}
		bl ReadJoystick
		tst r0,#0b00000100			; bit2 = LEFT
		moveq r0,#1
		movne r0,#0
	LDMFD sp!,{r2-r12,pc}

InputRight:
	STMFD sp!,{r2-r12,lr}
		bl ReadJoystick
		tst r0,#0b00001000			; bit3 = RIGHT
		moveq r0,#1
		movne r0,#0
	LDMFD sp!,{r2-r12,pc}

InputUp:
	STMFD sp!,{r2-r12,lr}
		bl ReadJoystick
		tst r0,#0b00000001			; bit0 = UP
		moveq r0,#1
		movne r0,#0
	LDMFD sp!,{r2-r12,pc}

InputDown:
	STMFD sp!,{r2-r12,lr}
		bl ReadJoystick
		tst r0,#0b00000010			; bit1 = DOWN
		moveq r0,#1
		movne r0,#0
	LDMFD sp!,{r2-r12,pc}

InputA:
	STMFD sp!,{r2-r12,lr}
		bl ReadJoystick
		tst r0,#0b00010000			; bit4 = A
		moveq r0,#1
		movne r0,#0
	LDMFD sp!,{r2-r12,pc}

InputB:
	STMFD sp!,{r2-r12,lr}
		bl ReadJoystick
		tst r0,#0b00100000			; bit5 = B
		moveq r0,#1
		movne r0,#0
	LDMFD sp!,{r2-r12,pc}
|}

let movement = {|
MoveLeft:
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r12,#StateBase		    ; Point to WRAM game state base
		ldrb r0,[r12,#CursorX_Off]	; Read current cursor X tile index
		cmp r0,#0					; Checks if were at left boundary?
		beq MoveLeftDone
		bl ShowCursor			    ; XOR hide old cursor (draw same sprite again)
		mov r12,#StateBase		    ; Reload base after subroutine calls
		ldrb r0,[r12,#CursorX_Off]	; Read X again
		sub r0,r0,#1				; X = X - 1 (move one tile left)
		strb r0,[r12,#CursorX_Off]	; Write updated X back to WRAM
		bl ShowCursor			    ; XOR show cursor at new location
MoveLeftDone:
	LDMFD sp!,{r2-r12,pc}			; Restore registers and return

MoveRight:
	; Same logic as MoveLeft, but boundary is x=2 and update is X = X + 1.
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r12,#StateBase		    ; Point to WRAM game state
		ldrb r0,[r12,#CursorX_Off]	; Read cursor X
		cmp r0,#2					; Checks if were at right boundary?
		beq MoveRightDone
		bl ShowCursor			    ; Remove the old cursor
		mov r12,#StateBase
		ldrb r0,[r12,#CursorX_Off]
		add r0,r0,#1				; X = X + 1
		strb r0,[r12,#CursorX_Off]	; Save new X
		bl ShowCursor			    ; Show the new cursor position
MoveRightDone:
	LDMFD sp!,{r2-r12,pc}			; Restore registers and return

MoveUp:
	; Same logic as MoveLeft, but operating on Y with top boundary y=0.
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r12,#StateBase		    ; Point to WRAM game state
		ldrb r0,[r12,#CursorY_Off]	; Read cursor Y
		cmp r0,#0					; Checks if were at top boundary?
		beq MoveUpDone
		bl ShowCursor			    ; Remove the old cursor
		mov r12,#StateBase
		ldrb r0,[r12,#CursorY_Off]
		sub r0,r0,#1				; Y = Y - 1
		strb r0,[r12,#CursorY_Off]	; Save new Y
		bl ShowCursor			    ; Show the new cursor position
MoveUpDone:
	LDMFD sp!,{r2-r12,pc}			; Restore registers and return

MoveDown:
	; Same logic as MoveLeft, but boundary is y=2 and update is Y = Y + 1.
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r12,#StateBase		    ; Point to WRAM game state
		ldrb r0,[r12,#CursorY_Off]	; Read cursor Y
		cmp r0,#2					; Checks if were at bottom boundary?
		beq MoveDownDone
		bl ShowCursor			    ; Remove the old cursor
		mov r12,#StateBase
		ldrb r0,[r12,#CursorY_Off]
		add r0,r0,#1				; Y = Y + 1
		strb r0,[r12,#CursorY_Off]	; Save new Y
		bl ShowCursor			    ; Show the new cursor position
MoveDownDone:
	LDMFD sp!,{r2-r12,pc}			; Restore registers and return

ShowCursor:
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r12,#StateBase		; Point to WRAM game state
		ldrb r0,[r12,#CursorX_Off]	; Read logical cursor X tile (0..2)
		ldrb r1,[r12,#CursorY_Off]	; Read logical cursor Y tile (0..2)

		mov r2,#80				; Tile width in pixels
		mul r8,r0,r2				; Pixel X base = tileX * 80
		add r8,r8,#40			    ; Move to tile center X

		mov r2,#53				; Tile height in pixels
		mul r9,r1,r2				; Pixel Y base = tileY * 53
		add r9,r9,#26			    ; Move to tile center Y

		sub r8,r8,#4			; Convert center X -> sprite top-left X (8x8 sprite)
		sub r9,r9,#4			; Convert center Y -> sprite top-left Y

		bl ShowSprite			; XOR draw/erase cursor sprite at (r8,r9)
	LDMFD sp!,{r2-r12,pc}			; Restore caller state and return
|}

let drawing = {|
InitGameState:
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r12,#StateBase		    ; r12 points to first state byte in WRAM
		mov r0,#0					; r0 holds value 0 (empty)
		mov r1,#GridCellsCount	    ; r1 is loop counter for 9 grid cells
InitGameStateLoop:
		strb r0,[r12],#1			; Write empty cell, then advance pointer by 1 byte
		subs r1,r1,#1				; Decrement remaining cells
		bne InitGameStateLoop

		mov r0,#0					; Reuse 0 for cursor initialization
		strb r0,[r12]				; Write CursorX = 0 at offset +9
		strb r0,[r12,#1]			; Write CursorY = 0 at offset +10

		mov r0,#CellX				; First turn starts as X
		strb r0,[r12,#2]			; Write CurrentSymbol = X at offset +11
	LDMFD sp!,{r2-r12,pc}			; Restore registers and return

DrawGrid:
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r3,#0x7F00				; Build white color constant high bits
		add r3,r3,#0xFF			    ; White color = 0x7FFF

		mov r1,#80				; Vertical separator #1 X position
		mov r2,#0				; Start at top row
		mov r4,#160				; Full screen height in pixels
		bl DrawVerticalLine		; Draw x=80 line

		mov r1,#160				; Vertical separator #2 X position
		mov r2,#0				; Start at top row
		mov r4,#160				; Full screen height
		bl DrawVerticalLine		; Draw x=160 line

		mov r1,#0				; Horizontal line starts at left edge
		mov r2,#53				; Horizontal separator #1 Y position
		mov r4,#240				; Full screen width in pixels
		bl DrawHorizontalLine	; Draw y=53 line

		mov r1,#0				; Horizontal line starts at left edge
		mov r2,#106				; Horizontal separator #2 Y position
		mov r4,#240				; Full screen width
		bl DrawHorizontalLine	; Draw y=106 line
	LDMFD sp!,{r2-r12,pc}			; Restore and return

TryPlaceSymbol:
	STMFD sp!,{r2-r12,lr}	    	; Preserve callee-saved regs
		mov r4,r0					; Preserve symbol argument from caller
		mov r0,#0					; Default return flag = 0 (Failed/Occupied)

		mov r12,#StateBase		    ; Point to WRAM state base
		ldrb r6,[r12,#CursorX_Off]	; r6 = cursor X tile (0..2)
		ldrb r7,[r12,#CursorY_Off]	; r7 = cursor Y tile (0..2)

		add r5,r7,r7,lsl #1		    ; r5 = y + (y<<1) = y*3
		add r5,r5,r6				; r5 = y*3 + x (final 1D board index 0..8)

		ldrb r3,[r12,r5]			; Read current board value at selected tile
		cmp r3,#CellEmpty			; Is tile empty?
		bne TryPlaceSymbolDone		; If occupied, abort and return r0=0

		bl ShowCursor			    ; XOR hide cursor before drawing X/O

		mov r12,#StateBase		    ; Reload WRAM state base
		strb r4,[r12,r5]			; Save logical symbol in board memory

		mov r0,r6					; r0 = tile X for renderer
		mov r1,r7					; r1 = tile Y for renderer
		mov r2,r4					; r2 = symbol ASCII value for renderer
		bl DrawSymbolInCell			; Draw scaled bitmap font character on screen

		bl ShowCursor				; XOR show cursor again
		mov r0,#1					; Set return flag to 1 (Success!)
TryPlaceSymbolDone:
	LDMFD sp!,{r2-r12,pc}		; Restore registers and return

DrawSymbolInCell:
	; IN: R0=CellX(0..2), R1=CellY(0..2), R2=ASCII symbol byte (eg. 'x','o','L','M')
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r3,#80					; Tile width in pixels (240/3)
		mul r6,r0,r3				; r6 = cellX * 80
		add r6,r6,#FontOffsetX		; Move to centered character start X inside tile

		mov r3,#53					; Tile height approx (160/3)
		mul r7,r1,r3				; r7 = cellY * 53
		add r7,r7,#FontOffsetY		; Move to centered character start Y inside tile

		mov r0,r2					; r0 = ASCII byte to render
		mov r1,r6					; r1 = absolute top-left X for the scaled character
		mov r2,r7					; r2 = absolute top-left Y for the scaled character
		bl DrawScaledChar			; Render 8x8 character scaled to FontScale x FontScale pixels
	LDMFD sp!,{r2-r12,pc}			; Restore geometry registers and return

; --- Revamped from Hello_World.asm ---
DrawScaledChar:
	; IN: R0=ASCII byte, R1=StartX, R2=StartY
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		adr r8,BitmapFont			; r8 points to first character (ASCII 32)
		sub r0,r0,#32				; Subtract space char so ASCII 32 maps to character index 0
		mov r0,r0,lsl #3			; Multiply index by 8 because each character stores 8 row bytes
		add r8,r8,r0				; Advance pointer to selected character first row byte

		mov r6,r1					; Keep character top-left X in r6 for all row/column math
		mov r7,r2					; Keep character top-left Y in r7 for all row/column math

		mov r9,#0					; r9 = character row index (0..7)
DrawScaledCharRowLoop:
		ldrb r4,[r8,r9]				; Read one character row byte (8 one-bit pixels)
		mov r5,#0x80				; Bit mask starts at left-most bit 1000 0000b
		mov r12,#0					; r12 = character column index (0..7)
DrawScaledCharColLoop:
		tst r4,r5					; Test current font bit: 1 means draw this pixel block
		beq DrawScaledCharNextCol	; If bit is 0 we skip drawing and move to next bit

		mov r3,#FontScale			; FontScale is used in both X and Y multiply math
		mul r1,r12,r3				; Scaled X offset = column * FontScale
		add r1,r1,r6				; Absolute X = characterStartX + scaled X offset
		mul r2,r9,r3				; Scaled Y offset = row * FontScale
		add r2,r2,r7				; Absolute Y = characterStartY + scaled Y offset

		bl DrawScaledPixel			; Draw one chunky white pixel (FontScale x FontScale)

DrawScaledCharNextCol:
		mov r5,r5,lsr #1			; Shift mask right so we test next bit in this row byte
		add r12,r12,#1				; Move to next character column
		cmp r12,#8					; Done all 8 bits for this row?
		bcc DrawScaledCharColLoop

		add r9,r9,#1				; Move to next character row byte
		cmp r9,#8					; Done all 8 rows for this character?
		bcc DrawScaledCharRowLoop
	LDMFD sp!,{r2-r12,pc}			; Restore caller state and return

DrawScaledPixel:
	; IN: R1=XStart, R2=YStart
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r3,#0x7F00				; Build white color high bits
		add r3,r3,#0xFF			    ; White color = 0x7FFF

		mov r4,#FontScale			; Horizontal width of one chunky scaled pixel
		mov r5,#FontScale			; Vertical height of one chunky scaled pixel
DrawScaledPixelRowLoop:
		bl DrawHorizontalLine		; Draw one horizontal slice of the chunky pixel
		add r2,r2,#1				; Move down one scanline inside this scaled pixel block
		subs r5,r5,#1				; Decrement remaining scanlines in this block
		bne DrawScaledPixelRowLoop
	LDMFD sp!,{r2-r12,pc}			; Restore scratch regs and return
|}

let gba_graphics = {|
DrawVerticalLine:
	; IN: R1=X, R2=YStart, R3=Color, R4=Height
	STMFD sp!,{r2-r12,lr}		; Preserve callee-saved regs
		bl GetScreenPos			; Convert (X,YStart) into VRAM pointer r10
		mov r6,r4				; r6 counts remaining pixels in this column
DrawVerticalLineLoop:
		strh r3,[r10]				; Write one 16-bit pixel color to VRAM
		add r10,r10,#240*2		    ; Move down exactly one scanline (480 bytes)
		subs r6,r6,#1				; Decrement remaining height
		bne DrawVerticalLineLoop
	LDMFD sp!,{r2-r12,pc}			; Restore and return

DrawHorizontalLine:
	; IN: R1=XStart, R2=Y, R3=Color, R4=Width
	STMFD sp!,{r2-r12,lr}		; Preserve callee-saved regs
		bl GetScreenPos			; Convert (XStart,Y) into VRAM pointer r10
		mov r6,r4				; r6 counts remaining pixels in this row
DrawHorizontalLineLoop:
		strh r3,[r10],#2			; Write one pixel, then move right by 2 bytes
		subs r6,r6,#1				; Decrement remaining width
		bne DrawHorizontalLineLoop
	LDMFD sp!,{r2-r12,pc}			; Restore and return

; --- Borrowed from Sprite_Moving.asm ---
;Xor Sprite, drawing twice will remove sprite from screen.
ShowSprite:
	STMFD sp!,{r2-r12,lr}		; Preserve callee-saved regs
	mov r10,#0x06000000 		; VRAM base address (Mode 3)

	mov r1,#2					; 2 bytes per 16-bit pixel
	mul r2,r1,r8				; r2 = x * 2 bytes
	add r10,r10,r2				; Move pointer to requested X position

	mov r1,#240*2				; One scanline = 240 pixels * 2 bytes
	mul r2,r1,r9				; r2 = y * 480 bytes
	add r10,r10,r2				; Move pointer to requested Y position

	ldr r1,SpriteAddress		; r1 points to sprite pixel data
	mov r6,#8					; Height loop count (8 rows)
Sprite_NextLine:
	mov r5,#8					; Width loop count (8 pixels per row)

	STMFD sp!,{r10}				; Save row start address before horizontal loop
Sprite_NextByte:
		ldrH r3,[r1],#2			; Read next sprite pixel and advance source pointer
		ldrH r2,[r10]			; Read current destination pixel from VRAM
		eor r3,r3,r2			; XOR sprite with screen pixel
		strH r3,[r10],#2		; Write XOR result and move right one pixel

		subs r5,r5,#1			; Decrement remaining pixels in this row
		bne Sprite_NextByte
	LDMFD sp!,{r10}				; Restore row start pointer
	add r10,r10,#240*2			; Move down one full scanline (480 bytes)
	subs r6,r6,#1				; Decrement remaining rows
	bne Sprite_NextLine			;Y loop
	LDMFD sp!,{r2-r12,pc}		; Restore all and return

SpriteAddress:
	.long CursorSprite			;Address of Sprite

; White cursor border, transparent center (0x0000)
CursorSprite:
	.word 0x7FFF,0x7FFF,0x7FFF,0x7FFF,0x7FFF,0x7FFF,0x7FFF,0x7FFF
	.word 0x7FFF,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x7FFF
	.word 0x7FFF,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x7FFF
	.word 0x7FFF,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x7FFF
	.word 0x7FFF,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x7FFF
	.word 0x7FFF,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x7FFF
	.word 0x7FFF,0x0000,0x0000,0x0000,0x0000,0x0000,0x0000,0x7FFF
	.word 0x7FFF,0x7FFF,0x7FFF,0x7FFF,0x7FFF,0x7FFF,0x7FFF,0x7FFF

; --- Borrowed from Hello_World.asm ---
BitmapFont:
	.byte 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00     ;  0 (technically starts from ASCII 32) 
	.byte 0x10,0x18,0x18,0x18,0x18,0x00,0x18,0x00     ;  1
	.byte 0x28,0x6C,0x28,0x00,0x00,0x00,0x00,0x00     ;  2
	.byte 0x00,0x28,0x7C,0x28,0x7C,0x28,0x00,0x00     ;  3
	.byte 0x18,0x3E,0x48,0x3C,0x12,0x7C,0x18,0x00     ;  4
	.byte 0x02,0xC4,0xC8,0x10,0x20,0x46,0x86,0x00     ;  5
	.byte 0x10,0x28,0x28,0x72,0x94,0x8C,0x72,0x00     ;  6
	.byte 0x0C,0x1C,0x30,0x00,0x00,0x00,0x00,0x00     ;  7
	.byte 0x18,0x18,0x30,0x30,0x30,0x18,0x18,0x00     ;  8
	.byte 0x18,0x18,0x0C,0x0C,0x0C,0x18,0x18,0x00     ;  9
	.byte 0x08,0x49,0x2A,0x1C,0x14,0x22,0x41,0x00     ; 10
	.byte 0x00,0x18,0x18,0x7E,0x18,0x18,0x00,0x00     ; 11
	.byte 0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x30     ; 12
	.byte 0x00,0x00,0x00,0x7E,0x7E,0x00,0x00,0x00     ; 13
	.byte 0x00,0x00,0x00,0x00,0x00,0x18,0x18,0x00     ; 14
	.byte 0x02,0x04,0x08,0x10,0x20,0x40,0x80,0x00     ; 15
	.byte 0x7C,0xC6,0xD6,0xD6,0xD6,0xC6,0x7C,0x00     ; 16
	.byte 0x10,0x18,0x18,0x18,0x18,0x18,0x08,0x00     ; 17
	.byte 0x3C,0x7E,0x06,0x3C,0x60,0x7E,0x3C,0x00     ; 18
	.byte 0x3C,0x7E,0x06,0x1C,0x06,0x7E,0x3C,0x00     ; 19
	.byte 0x18,0x3C,0x64,0xCC,0x7C,0x0C,0x08,0x00     ; 20
	.byte 0x3C,0x7E,0x60,0x7C,0x06,0x7E,0x3E,0x00     ; 21
	.byte 0x3C,0x7E,0x60,0x7C,0x66,0x66,0x3C,0x00     ; 22
	.byte 0x3C,0x7E,0x06,0x0C,0x18,0x18,0x10,0x00     ; 23
	.byte 0x3C,0x66,0x66,0x3C,0x66,0x66,0x3C,0x00     ; 24
	.byte 0x3C,0x66,0x66,0x3E,0x06,0x7E,0x3C,0x00     ; 25
	.byte 0x00,0x00,0x18,0x18,0x00,0x18,0x18,0x00     ; 26
	.byte 0x00,0x00,0x18,0x18,0x00,0x18,0x18,0x30     ; 27
	.byte 0x0C,0x1C,0x38,0x60,0x38,0x1C,0x0C,0x00     ; 28
	.byte 0x00,0x00,0x7E,0x00,0x00,0x7E,0x00,0x00     ; 29
	.byte 0x60,0x70,0x38,0x0C,0x38,0x70,0x60,0x00     ; 30
	.byte 0x3C,0x76,0x06,0x1C,0x00,0x18,0x18,0x00     ; 31
	.byte 0x7C,0xCE,0xA6,0xB6,0xC6,0xF0,0x7C,0x00     ; 32
	.byte 0x18,0x3C,0x66,0x66,0x7E,0x66,0x24,0x00     ; 33
	.byte 0x3C,0x66,0x66,0x7C,0x66,0x66,0x3C,0x00     ; 34
	.byte 0x38,0x7C,0xC0,0xC0,0xC0,0x7C,0x38,0x00     ; 35
	.byte 0x3C,0x64,0x66,0x66,0x66,0x64,0x38,0x00     ; 36
	.byte 0x3C,0x7E,0x60,0x78,0x60,0x7E,0x3C,0x00     ; 37
	.byte 0x38,0x7C,0x60,0x78,0x60,0x60,0x20,0x00     ; 38
	.byte 0x3C,0x66,0xC0,0xC0,0xCC,0x66,0x3C,0x00     ; 39
	.byte 0x24,0x66,0x66,0x7E,0x66,0x66,0x24,0x00     ; 40
	.byte 0x10,0x18,0x18,0x18,0x18,0x18,0x08,0x00     ; 41
	.byte 0x08,0x0C,0x0C,0x0C,0x4C,0xFC,0x78,0x00     ; 42
	.byte 0x24,0x66,0x6C,0x78,0x6C,0x66,0x24,0x00     ; 43
	.byte 0x20,0x60,0x60,0x60,0x60,0x7E,0x3E,0x00     ; 44
	.byte 0x44,0xEE,0xFE,0xD6,0xD6,0xD6,0x44,0x00     ; 45
	.byte 0x44,0xE6,0xF6,0xDE,0xCE,0xC6,0x44,0x00     ; 46
	.byte 0x38,0x6C,0xC6,0xC6,0xC6,0x6C,0x38,0x00     ; 47
	.byte 0x38,0x6C,0x64,0x7C,0x60,0x60,0x20,0x00     ; 48
	.byte 0x38,0x6C,0xC6,0xC6,0xCA,0x74,0x3A,0x00     ; 49
	.byte 0x3C,0x66,0x66,0x7C,0x6C,0x66,0x26,0x00     ; 50
	.byte 0x3C,0x7E,0x60,0x3C,0x06,0x7E,0x3C,0x00     ; 51
	.byte 0x3C,0x7E,0x18,0x18,0x18,0x18,0x08,0x00     ; 52
	.byte 0x24,0x66,0x66,0x66,0x66,0x66,0x3C,0x00     ; 53
	.byte 0x24,0x66,0x66,0x66,0x66,0x3C,0x18,0x00     ; 54
	.byte 0x44,0xC6,0xD6,0xD6,0xFE,0xEE,0x44,0x00     ; 55
	.byte 0xC6,0x6C,0x38,0x38,0x6C,0xC6,0x44,0x00     ; 56
	.byte 0x24,0x66,0x66,0x3C,0x18,0x18,0x08,0x00     ; 57
	.byte 0x7C,0xFC,0x0C,0x18,0x30,0x7E,0x7C,0x00     ; 58
	.byte 0x1C,0x30,0x30,0x30,0x30,0x30,0x1C,0x00     ; 59
	.byte 0x80,0x40,0x20,0x10,0x08,0x04,0x02,0x00     ; 60
	.byte 0x38,0x0C,0x0C,0x0C,0x0C,0x0C,0x38,0x00     ; 61
	.byte 0x18,0x18,0x18,0x18,0x7E,0x7E,0x18,0x18     ; 62
	.byte 0x18,0x18,0x18,0x18,0x3C,0x3C,0x18,0x18     ; 63
	.byte 0x18,0x18,0x18,0x18,0x18,0x18,0x18,0x18     ; 64
	.byte 0x00,0x00,0x38,0x0C,0x7C,0xCC,0x78,0x00     ; 65
	.byte 0x20,0x60,0x7C,0x66,0x66,0x66,0x3C,0x00     ; 66
	.byte 0x00,0x00,0x3C,0x66,0x60,0x66,0x3C,0x00     ; 67
	.byte 0x08,0x0C,0x7C,0xCC,0xCC,0xCC,0x78,0x00     ; 68
	.byte 0x00,0x00,0x3C,0x66,0x7E,0x60,0x3C,0x00     ; 69
	.byte 0x1C,0x36,0x30,0x38,0x30,0x30,0x10,0x00     ; 70
	.byte 0x00,0x00,0x3C,0x66,0x66,0x3E,0x06,0x3C     ; 71
	.byte 0x20,0x60,0x6C,0x76,0x66,0x66,0x24,0x00     ; 72
	.byte 0x18,0x00,0x18,0x18,0x18,0x18,0x08,0x00     ; 73
	.byte 0x06,0x00,0x04,0x06,0x06,0x26,0x66,0x3C     ; 74
	.byte 0x20,0x60,0x66,0x6C,0x78,0x6C,0x26,0x00     ; 75
	.byte 0x10,0x18,0x18,0x18,0x18,0x18,0x08,0x00     ; 76
	.byte 0x00,0x00,0x6C,0xFE,0xD6,0xD6,0xC6,0x00     ; 77
	.byte 0x00,0x00,0x3C,0x66,0x66,0x66,0x24,0x00     ; 78
	.byte 0x00,0x00,0x3C,0x66,0x66,0x66,0x3C,0x00     ; 79
	.byte 0x00,0x00,0x3C,0x66,0x66,0x7C,0x60,0x20     ; 80
	.byte 0x00,0x00,0x78,0xCC,0xCC,0x7C,0x0C,0x08     ; 81
	.byte 0x00,0x00,0x38,0x7C,0x60,0x60,0x20,0x00     ; 82
	.byte 0x00,0x00,0x3C,0x60,0x3C,0x06,0x7C,0x00     ; 83
	.byte 0x10,0x30,0x3C,0x30,0x30,0x3E,0x1C,0x00     ; 84
	.byte 0x00,0x00,0x24,0x66,0x66,0x66,0x3C,0x00     ; 85
	.byte 0x00,0x00,0x24,0x66,0x66,0x3C,0x18,0x00     ; 86
	.byte 0x00,0x00,0x44,0xD6,0xD6,0xFE,0x6C,0x00     ; 87
	.byte 0x00,0x00,0xC6,0x6C,0x38,0x6C,0xC6,0x00     ; 88
	.byte 0x00,0x00,0x24,0x66,0x66,0x3E,0x06,0x7C     ; 89
	.byte 0x00,0x00,0x7E,0x0C,0x18,0x30,0x7E,0x00     ; 90
	.byte 0x08,0x08,0x08,0x08,0x56,0x55,0x57,0x74     ; 91
	.byte 0x18,0x04,0x08,0x1C,0x56,0x55,0x57,0x74     ; 92
	.byte 0x00,0x00,0x00,0x00,0x7E,0x7E,0xFF,0xFF     ; 93
	.byte 0x18,0x3C,0x18,0x18,0x18,0x18,0x7E,0xFF     ; 94
	.byte 0x22,0x77,0x7F,0x7F,0x3E,0x1C,0x08,0x00     ; 95 (technically goes up to ASCII 127)
|}

let gba_hardware = {|
; --- Borrowed from Game_Example.asm ---
ReadJoystick:		
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
		mov r3,#0x4000130			; KEYINPUT register address
		ldrh r2,[r3]				; Raw bits:  (0 = pressed)
		and r1,r2,#0b0000000011000000	; Extract Up/Down bits
		mov r0,r1,lsr #6			; Place U/D into output bits 0..1
		and r1,r2,#0b0000000000100000	; Extract Left bit
		orr r0,r0,r1,lsr #3		; Place L into output bit 2
		and r1,r2,#0b0000000000010000	; Extract Right bit
		orr r0,r0,r1,lsr #1		; Place R into output bit 3
		and r1,r2,#0b0000000000001111	; Extract SsBA bits
		orr r0,r0,r1,lsl #4		; Place SsBA into output bits 4..7
	LDMFD sp!,{r2-r12,pc}			; Return with normalized SSBARLDU in r0

; --- Borrowed from Game_Example.asm (button release pattern) ---
; --- Input format relies on ReadJoystick mapping from Game_Example.asm ---
WaitForReleaseAny:
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
WaitForReleaseAnyLoop:
		bl ReadJoystick			    ; Read current normalized input bits
		and r1,r0,#0b00011111		; Isolate RLDU+A bits only (bits 0..4)
		cmp r1,#0b00011111		    ; All 1 means all released (active-low scheme)
		bne WaitForReleaseAnyLoop	;Repeat until RLDU+A released
	LDMFD sp!,{r2-r12,pc}		    ; Return once release condition is met

; --- Borrowed/Adapted from Joypad_Controls.asm (VBlank) ---
; --- Integrated with button processing flow from Game_Example.asm ---
WaitVBlankStart:
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
Delay:
	mov r0,#0x4000004			; DISPSTAT register address
	ldrh r1,[r0]				; Read LCD status flags
	ands r1,r1,#1				; Test VBlank flag (bit0)
	bne Delay				    ; Stay here while currently in VBlank
Delay2:
	mov r0,#0x4000004			; DISPSTAT register address
	ldrh r1,[r0]				; Read LCD status again
	ands r1,r1,#1				; Test VBlank flag (bit0)
	beq Delay2				    ; Wait until next VBlank begins
	LDMFD sp!,{r2-r12,pc}		; Restore regs and return

; --- Borrowed from Bitmap_Graphics_16_bit.asm ---
GetScreenPos:
	; IN: R1 = X, R2 = Y
	; OUT: R10 = Screen Address
	STMFD sp!,{r2-r9,r11-r12,lr}	; Preserve callee-saved regs (exclude r10)
		mov r10,#0x06000000		; Start from Mode 3 VRAM base
		mov r3,#240*2			; Bytes per scanline (240 pixels * 2 bytes)
		mul r4,r3,r2			; r4 = Y offset in bytes
		add r10,r10,r1,lsl #1		; Add X offset in bytes (X*2)
		add r10,r10,r4			; Add Y offset in bytes
	LDMFD sp!,{r2-r9,r11-r12,pc}	; Restore regs and return

; --- Borrowed from Bitmap_Graphics_16_bit.asm ---
ScreenInit:
	STMFD sp!,{r2-r12, lr}		; Preserve callee-saved regs
		mov r4,#0x04000000  	; DISPCNT register address
		mov r2,#0x403    		; Mode3 + BG2 enable
		str	r2,[r4]				; Write display mode configuration

		bl cls					; Clear framebuffer to black

	LDMFD sp!,{r2-r12, pc}		; Restore and return

; --- Borrowed from Bitmap_Graphics_16_bit.asm (black background adapted) ---
cls:
	STMFD sp!,{r2-r12,lr}			; Preserve callee-saved regs
	mov r0, #0x06000000			; Destination pointer = Mode 3 VRAM base
	mov r1, #0x00000000			; Fill value = black (two black pixels per 32-bit write)
	mov r2, #240*160/2			; Number of 32-bit writes to cover full screen

FillScreenLoop:
		str r1, [r0],#4			; Write two pixels, then advance pointer by 4 bytes
		subs r2, r2, #1			; Decrement remaining write count
		bne FillScreenLoop
	LDMFD sp!,{r2-r12,pc}			; Restore regs and return
|}
