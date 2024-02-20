// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(LOOP)

@KBD
D=M

@FILL
D;JGT

@CLEAR
D;JEQ

(FILL)
	@SCREEN
	D=A // D = 16384

	@i
	M=D	// i = 16384

	(LOOP-FILL)
		@i
		A=M
		M=-1  // -1 = 0b1111111111111111
		
		@i
		M=M+1 // i = i + 1
		
		@i
		D=M  // D = i

		@24575
		D=D-A // i = i - 24575
		
		@LOOP-FILL
		D;JLE
	
	@LOOP
	0;JMP

(CLEAR)
	@SCREEN
	D=A // D = 16384

	@i
	M=D	// i = 16384

	(LOOP-CLEAR)
		@i
		A=M
		M=0
		
		@i
		M=M+1 // i = i + 1
		
		@i
		D=M  // D = i

		@24575
		D=D-A // i = i - 24575
		
		@LOOP-CLEAR
		D;JLE

	@LOOP
	0;JMP
