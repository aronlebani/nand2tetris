@17
D=A
@SP
A=M
M=D
@SP
M=M+1



@17
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@TRUE_P.1
D;JEQ
@FALSE_P.1
0;JMP
(TRUE_P.1)
D=-1
@END.1
0;JMP
(FALSE_P.1)
D=0
@END.1
0;JMP
(END.1)

@SP
A=M
M=D
@SP
M=M+1



@17
D=A
@SP
A=M
M=D
@SP
M=M+1



@16
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@TRUE_P.2
D;JEQ
@FALSE_P.2
0;JMP
(TRUE_P.2)
D=-1
@END.2
0;JMP
(FALSE_P.2)
D=0
@END.2
0;JMP
(END.2)

@SP
A=M
M=D
@SP
M=M+1



@16
D=A
@SP
A=M
M=D
@SP
M=M+1



@17
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@TRUE_P.3
D;JEQ
@FALSE_P.3
0;JMP
(TRUE_P.3)
D=-1
@END.3
0;JMP
(FALSE_P.3)
D=0
@END.3
0;JMP
(END.3)

@SP
A=M
M=D
@SP
M=M+1



@892
D=A
@SP
A=M
M=D
@SP
M=M+1



@891
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@TRUE_P.4
D;JLT
@FALSE_P.4
0;JMP
(TRUE_P.4)
D=-1
@END.4
0;JMP
(FALSE_P.4)
D=0
@END.4
0;JMP
(END.4)

@SP
A=M
M=D
@SP
M=M+1



@891
D=A
@SP
A=M
M=D
@SP
M=M+1



@892
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@TRUE_P.5
D;JLT
@FALSE_P.5
0;JMP
(TRUE_P.5)
D=-1
@END.5
0;JMP
(FALSE_P.5)
D=0
@END.5
0;JMP
(END.5)

@SP
A=M
M=D
@SP
M=M+1



@891
D=A
@SP
A=M
M=D
@SP
M=M+1



@891
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@TRUE_P.6
D;JLT
@FALSE_P.6
0;JMP
(TRUE_P.6)
D=-1
@END.6
0;JMP
(FALSE_P.6)
D=0
@END.6
0;JMP
(END.6)

@SP
A=M
M=D
@SP
M=M+1



@32767
D=A
@SP
A=M
M=D
@SP
M=M+1



@32766
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@TRUE_P.7
D;JGT
@FALSE_P.7
0;JMP
(TRUE_P.7)
D=-1
@END.7
0;JMP
(FALSE_P.7)
D=0
@END.7
0;JMP
(END.7)

@SP
A=M
M=D
@SP
M=M+1



@32766
D=A
@SP
A=M
M=D
@SP
M=M+1



@32767
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@TRUE_P.8
D;JGT
@FALSE_P.8
0;JMP
(TRUE_P.8)
D=-1
@END.8
0;JMP
(FALSE_P.8)
D=0
@END.8
0;JMP
(END.8)

@SP
A=M
M=D
@SP
M=M+1



@32766
D=A
@SP
A=M
M=D
@SP
M=M+1



@32766
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@TRUE_P.9
D;JGT
@FALSE_P.9
0;JMP
(TRUE_P.9)
D=-1
@END.9
0;JMP
(FALSE_P.9)
D=0
@END.9
0;JMP
(END.9)

@SP
A=M
M=D
@SP
M=M+1



@57
D=A
@SP
A=M
M=D
@SP
M=M+1



@31
D=A
@SP
A=M
M=D
@SP
M=M+1



@53
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M+D
@SP
A=M
M=D
@SP
M=M+1



@112
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M-D
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=-M
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M&D
@SP
A=M
M=D
@SP
M=M+1



@82
D=A
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=M

@SP
M=M-1

@SP
A=M

D=M|D
@SP
A=M
M=D
@SP
M=M+1



@SP
M=M-1

@SP
A=M

D=!M
@SP
A=M
M=D
@SP
M=M+1



