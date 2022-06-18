DATASEG
    object_vector_type dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw offset object_vector_eq
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw offset object_vector_show

    ; Vector related variables
    vector_print_start db "(", 0
    vector_print_middle db ", ", 0
    vector_print_end db ")", 0

    ; Vector offsets
    OBJECT_VECTOR_OFF_X = 4
    OBJECT_VECTOR_OFF_Y = 6

CODESEG
; Vector object

; Get the x of the vector
object_ptr = bp + 4
proc object_vector_get_x
    push bp
    mov bp, sp
    push es

    mov ax, [object_ptr]
    mov es, ax

    mov ax, [es:OBJECT_VECTOR_OFF_X]

    pop es
    pop bp
    ret 2
endp object_vector_get_x

; Get the y of the vector
object_ptr = bp + 4
proc object_vector_get_y
    push bp
    mov bp, sp
    push es

    mov ax, [object_ptr]
    mov es, ax

    mov ax, [es:OBJECT_VECTOR_OFF_Y]

    pop es
    pop bp
    ret 2
endp object_vector_get_y

; Shows a representation of a vector
object_ptr = bp + 4
proc object_vector_show
    push bp
    mov bp, sp
    push ax

    push offset vector_print_start
    call print_data_cstr

    push [object_ptr]
    call object_vector_get_x
    push ax
    call print_word

    push offset vector_print_middle
    call print_data_cstr

    push [object_ptr]
    call object_vector_get_y
    push ax
    call print_word

    push offset vector_print_end
    call print_data_cstr

    pop ax
    pop bp
    ret 2
endp object_vector_show

; Return whether the two argument vectors are equal
lhs_ptr = bp + 4
rhs_ptr = bp + 6
lhs_number = bp - 2
rhs_number = bp - 4
proc object_vector_eq
    push bp
    mov bp, sp
    sub sp, 4

    ; Store x of lhs
    push [lhs_ptr]
    call object_vector_get_x
    mov [lhs_number], ax
    ; Store x of rhs
    push [rhs_ptr]
    call object_vector_get_x
    mov [rhs_number], ax

    mov ax, [lhs_number]
    cmp ax, [rhs_number]
    jne @@not_equal

    ; Store y of lhs
    push [lhs_ptr]
    call object_vector_get_y
    mov [lhs_number], ax
    ; Store y of rhs
    push [rhs_ptr]
    call object_vector_get_y
    mov [rhs_number], ax

    mov ax, [lhs_number]
    cmp ax, [rhs_number]
    jne @@not_equal

    ; If we got here, the vectors are equal
    mov ax, 1
    jmp @@end_equality_check

@@not_equal:
    mov ax, 0

@@end_equality_check:

    add sp, 4
    pop bp
    ret 4
endp object_vector_eq
