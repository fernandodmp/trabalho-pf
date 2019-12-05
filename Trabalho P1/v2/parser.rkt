#lang brag
bf-program : (bf-expr | bf-loop)*
bf-expr: (OPERAND INTEGER | OPERAND)
bf-loop: "begin" (bf-expr | bf-loop)* "end"