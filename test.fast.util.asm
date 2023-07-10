; executes a null teminated string (null executes exit_)
; the string should be immedaitely following the call
execStr:                       ; create a root stack frame
    pop bc                     ; bc = code*
    dec bc                     ; dec to prepare for next routine
    ld de,0
    push de                    ; push fake IP
    push de                    ; push null arg_list*
    push de                    ; push null first_arg*
    push de                    ; push fake BP
    jp (ix) 
