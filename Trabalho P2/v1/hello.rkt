#lang reader "reader.rkt"
move $ACC 72
store
inc $SP
move $ACC 101
store
inc $SP
move $ACC 108
store
inc $SP
store
inc $SP
move $ACC 111
store
inc $SP
move $ACC 33
store
move $CR 0
move $BR 5
sub $SP $BR
load
write
inc $SP
inc $CR
jl 19 $CR $BR
