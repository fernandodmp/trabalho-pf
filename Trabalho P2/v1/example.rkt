#lang reader "reader.rkt"
read
move $DR $ACC
sub $DR 48
read
inc $ACC
store
inc $SP
jne 3 $SP $DR
move $SP 0
move $CR $DR
move $BR 0
load
write
inc $SP
dec $CR
jg 11 $CR $BR