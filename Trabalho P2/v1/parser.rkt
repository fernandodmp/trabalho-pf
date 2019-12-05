#lang brag
asm-program : expr*
expr: io-expr | mem-expr | op-expr
io-expr: IO-OP
mem-expr: MEM-OP
op-expr: single-op | double-op | jmp-expr
single-op: SINGLE-OP REGISTER
double-op: DOUBLE-OP REGISTER (REGISTER | INTEGER)
jmp-expr: (jmp | jmpc)
jmp: JUMP-OP INTEGER
jmpc: JUMPC-OP INTEGER REGISTER REGISTER
