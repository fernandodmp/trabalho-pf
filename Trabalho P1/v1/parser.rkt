#lang brag
bf-program : (bf-op | bf-loop)*
bf-op      : "fwd" | "rwd" | "inc" | "dec" | "write" | "read"
bf-loop    : "begin" (bf-op | bf-loop)* "end"