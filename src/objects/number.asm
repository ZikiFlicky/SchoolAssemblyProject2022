DATASEG
    object_number_type dw 0
                       dw offset object_number_add
                       dw offset object_number_sub
                       dw offset object_number_mul
                       dw offset object_number_div
                       dw offset object_number_mod
                       dw offset object_number_eq
                       dw offset object_number_smaller
                       dw offset object_number_bigger
                       dw offset object_number_smaller_eq
                       dw offset object_number_bigger_eq
                       dw offset object_number_neg
                       dw offset object_number_chr
                       dw offset object_number_to_bool
                       dw offset object_number_show

    ; Number offsets
    OBJECT_NUMBER_OFF_NUMBER = 4

CODESEG
; Number operations

; Defines the `+` binary operator used in the number binary operations
lhs = bp + 4
rhs = bp + 6
expr_ptr = bp + 8
proc operator_add_func
    push bp
    mov bp, sp

    mov ax, [lhs]
    add ax, [rhs]

    pop bp
    ret 6
endp operator_add_func

; Defines the `-` binary operator used in the number binary operations
lhs = bp + 4
rhs = bp + 6
expr_ptr = bp + 8
proc operator_sub_func
    push bp
    mov bp, sp

    mov ax, [lhs]
    sub ax, [rhs]

    pop bp
    ret 6
endp operator_sub_func

; Defines the `*` binary operator used in the number binary operations
lhs = bp + 4
rhs = bp + 6
expr_ptr = bp + 8
proc operator_mul_func
    push bp
    mov bp, sp
    push bx
    push dx
    push es

    mov ax, [expr_ptr]
    mov es, ax

    xor dx, dx
    mov ax, [lhs]
    mov bx, [rhs]
    imul bx

    cmp dx, 0
    je @@not_overflowed
    cmp dx, -1
    je @@not_overflowed

    ; If we got here, we overflowed
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_mul_overflow
    call runtime_error

@@not_overflowed:

    pop es
    pop dx
    pop bx
    pop bp
    ret 6
endp operator_mul_func

; Defines the `/` binary operator used in the number binary operations
lhs = bp + 4
rhs = bp + 6
expr_ptr = bp + 8
proc operator_div_func
    push bp
    mov bp, sp
    push bx
    push dx
    push es

    cmp [word ptr rhs], 0
    jne @@not_div_by_zero

    ; If we got here, we divide by 0
    mov ax, [expr_ptr]
    mov es, ax

    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_div_by_zero
    call runtime_error

@@not_div_by_zero:
    mov ax, [lhs]
    mov bx, [rhs]

    cmp [word ptr lhs], 0
    jl @@prepare_negative
    mov dx, 0
    jmp @@do_div

@@prepare_negative:
    mov dx, -1

@@do_div:
    idiv bx

    pop es
    pop dx
    pop bx
    pop bp
    ret 6
endp operator_div_func

; Defines the `%` binary operator used in the number binary operations
lhs = bp + 4
rhs = bp + 6
expr_ptr = bp + 8
proc operator_mod_func
    push bp
    mov bp, sp
    push bx
    push dx
    push es

    cmp [word ptr rhs], 0
    jne @@not_div_by_zero

    ; If we got here, we divide by 0
    mov ax, [expr_ptr]
    mov es, ax

    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_div_by_zero
    call runtime_error

@@not_div_by_zero:
    mov ax, [lhs]
    mov bx, [rhs]

    cmp [word ptr lhs], 0
    jl @@prepare_negative
    mov dx, 0
    jmp @@do_div

@@prepare_negative:
    mov dx, -1

@@do_div:
    idiv bx

    ; Return the remainder
    mov ax, dx

    pop es
    pop dx
    pop bx
    pop bp
    ret 6
endp operator_mod_func

; Defines the `<` binary operator used in the number binary operations
lhs = bp + 4
rhs = bp + 6
expr_ptr = bp + 8
proc operator_cmp_smaller_func
    push bp
    mov bp, sp

    mov ax, [lhs]
    cmp ax, [rhs]
    jl @@smaller

    mov ax, 0
    jmp @@end_cmp

@@smaller:
    mov ax, 1

@@end_cmp:

    pop bp
    ret 6
endp operator_cmp_smaller_func

; Defines the `>` binary operator used in the number binary operations
lhs = bp + 4
rhs = bp + 6
expr_ptr = bp + 8
proc operator_cmp_bigger_func
    push bp
    mov bp, sp

    mov ax, [lhs]
    cmp ax, [rhs]
    jg @@bigger

    mov ax, 0
    jmp @@end_cmp

@@bigger:
    mov ax, 1

@@end_cmp:

    pop bp
    ret 6
endp operator_cmp_bigger_func

; Defines the `<=` binary operator used in the number binary operations
lhs = bp + 4
rhs = bp + 6
expr_ptr = bp + 8
proc operator_cmp_smaller_equals_func
    push bp
    mov bp, sp

    mov ax, [lhs]
    cmp ax, [rhs]
    jle @@smaller_equals

    mov ax, 0
    jmp @@end_cmp

@@smaller_equals:
    mov ax, 1

@@end_cmp:

    pop bp
    ret 6
endp operator_cmp_smaller_equals_func

; Defines the `>=` binary operator used in the number binary operations
lhs = bp + 4
rhs = bp + 6
expr_ptr = bp + 8
proc operator_cmp_bigger_equals_func
    push bp
    mov bp, sp

    mov ax, [lhs]
    cmp ax, [rhs]
    jge @@bigger_equals

    mov ax, 0
    jmp @@end_cmp

@@bigger_equals:
    mov ax, 1

@@end_cmp:

    pop bp
    ret 6
endp operator_cmp_bigger_equals_func

; Number object

; Shows a representation of the number object given as parameter
object_ptr = bp + 4
proc object_number_show
    push bp
    mov bp, sp
    push ax

    push [object_ptr]
    call object_number_get

    push ax
    call print_word

    pop ax
    pop bp
    ret 2
endp object_number_show

; Calls the function on the two number objects
; Used for the other binary number operations
operator_func = bp + 4
lhs_value = bp + 6
rhs_value = bp + 8
expr_ptr = bp + 10
lhs_number = bp - 2
rhs_number = bp - 4
result = bp - 6
proc object_number_binary_operation
    push bp
    mov bp, sp
    sub sp, 6
    push es

    push [lhs_value]
    call object_number_get
    mov [lhs_number], ax

    push [rhs_value]
    call object_number_get
    mov [rhs_number], ax

    ; Call the operator function
    mov ax, [operator_func]
    push [expr_ptr]
    push [rhs_number]
    push [lhs_number]
    call ax
    ; Create a number from the return value
    push ax
    call object_number_new

    pop es
    add sp, 6
    pop bp
    ret 8
endp object_number_binary_operation

; Define a `+` method/function for the number object
lhs_value = bp + 4
rhs_value = bp + 6
expr_ptr = bp + 8
proc object_number_add
    push bp
    mov bp, sp

    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    push offset operator_add_func
    call object_number_binary_operation

    pop bp
    ret 6
endp object_number_add

; Define a `-` method/function for the number object
lhs_value = bp + 4
rhs_value = bp + 6
expr_ptr = bp + 8
proc object_number_sub
    push bp
    mov bp, sp

    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    push offset operator_sub_func
    call object_number_binary_operation

    pop bp
    ret 6
endp object_number_sub

; Define a `*` method/function for the number object
lhs_value = bp + 4
rhs_value = bp + 6
expr_ptr = bp + 8
proc object_number_mul
    push bp
    mov bp, sp

    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    push offset operator_mul_func
    call object_number_binary_operation

    pop bp
    ret 6
endp object_number_mul

; Define a `/` method/function for the number object
lhs_value = bp + 4
rhs_value = bp + 6
expr_ptr = bp + 8
proc object_number_div
    push bp
    mov bp, sp

    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    push offset operator_div_func
    call object_number_binary_operation

    pop bp
    ret 6
endp object_number_div

; Define a `%` method/function for the number object
lhs_value = bp + 4
rhs_value = bp + 6
expr_ptr = bp + 8
proc object_number_mod
    push bp
    mov bp, sp

    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    push offset operator_mod_func
    call object_number_binary_operation

    pop bp
    ret 6
endp object_number_mod

; Define a `==` method/function for two number objects
lhs_value = bp + 4
rhs_value = bp + 6
lhs_number = bp - 2
rhs_number = bp - 4
proc object_number_eq
    push bp
    mov bp, sp
    sub sp, 4
    push es

    push [lhs_value]
    call object_number_get
    mov [lhs_number], ax

    push [rhs_value]
    call object_number_get
    mov [rhs_number], ax

    mov ax, [lhs_number]
    cmp ax, [rhs_number]
    jne @@not_equal

    mov ax, 1
    jmp @@end_cmp

@@not_equal:
    mov ax, 0

@@end_cmp:

    pop es
    add sp, 4
    pop bp
    ret 4
endp object_number_eq

; Define a `<` method/function for the number object
lhs_value = bp + 4
rhs_value = bp + 6
expr_ptr = bp + 8
proc object_number_smaller
    push bp
    mov bp, sp

    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    push offset operator_cmp_smaller_func
    call object_number_binary_operation

    pop bp
    ret 6
endp object_number_smaller

; Define a `>` method/function for the number object
lhs_value = bp + 4
rhs_value = bp + 6
expr_ptr = bp + 8
proc object_number_bigger
    push bp
    mov bp, sp

    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    push offset operator_cmp_bigger_func
    call object_number_binary_operation

    pop bp
    ret 6
endp object_number_bigger

; Define a `<=` method/function for the number object
lhs_value = bp + 4
rhs_value = bp + 6
expr_ptr = bp + 8
proc object_number_smaller_eq
    push bp
    mov bp, sp

    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    push offset operator_cmp_smaller_equals_func
    call object_number_binary_operation

    pop bp
    ret 6
endp object_number_smaller_eq

; Define a `>=` method/function for the number object
lhs_value = bp + 4
rhs_value = bp + 6
expr_ptr = bp + 8
proc object_number_bigger_eq
    push bp
    mov bp, sp

    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    push offset operator_cmp_bigger_equals_func
    call object_number_binary_operation

    pop bp
    ret 6
endp object_number_bigger_eq

; Define a negative (`-`) method/function for the number object
object_ptr = bp + 4
proc object_number_neg
    push bp
    mov bp, sp
    push es

    push [object_ptr]
    call object_number_get
    neg ax
    ; Create a number from the negative number
    push ax
    call object_number_new

    pop es
    pop bp
    ret 2
endp object_number_neg

; Define a chr method/function for the number object
object_ptr = bp + 4
number = bp - 2
proc object_number_chr
    push bp
    mov bp, sp
    sub sp, 2

    push [object_ptr]
    call object_number_get
    mov [number], ax

    ; Check that the number is byte-like (between 0 and 255)
    push 256
    push 0
    push ax
    call number_validate
    test ax, ax
    jz @@error_number_invalid

    ; Create a single-char number from the byte-like word
    push [number]
    call object_string_single_char_new
    jmp @@end_run

@@error_number_invalid:
    push offset error_message_chr_number_invalid
    call runtime_error_no_state

@@end_run:

    add sp, 2
    pop bp
    ret 2
endp object_number_chr

; Returns whether number is truthy
object_ptr = bp + 4
proc object_number_to_bool
    push bp
    mov bp, sp

    push [object_ptr]
    call object_number_get
    test ax, ax

    test ax, ax
    jnz @@object_truthy

    mov ax, 0
    jmp @@object_check_end

@@object_truthy:
    mov ax, 1

@@object_check_end:

    pop bp
    ret 2
endp object_number_to_bool

; Returns the word stored in the number object
object_ptr = bp + 4
proc object_number_get
    push bp
    mov bp, sp
    push es

    mov ax, [object_ptr]
    mov es, ax

    mov ax, [es:OBJECT_NUMBER_OFF_NUMBER]

    pop es
    pop bp
    ret 2
endp object_number_get

; Create a new number with the given word
number = bp + 4
proc object_number_new
    push bp
    mov bp, sp
    push es

    ; Move the new object pointer to es
    push offset object_number_type
    call object_new
    mov es, ax

    ; Set the given value as the object's value
    mov ax, [number]
    mov [es:OBJECT_NUMBER_OFF_NUMBER], ax

    ; Return the object
    mov ax, es

    pop es
    pop bp
    ret 2
endp object_number_new

