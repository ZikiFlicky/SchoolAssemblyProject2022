DATASEG
    object_string_type dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw offset object_string_eq
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw 0
                       dw offset object_string_chr
                       dw offset object_string_to_bool
                       dw offset object_string_show

    ; String offsets
    OBJECT_STRING_OFF_SOURCE = 4
    OBJECT_STRING_OFF_LENGTH = 6

CODESEG
; String object

; Shows a representation of the string object given as parameter
object_ptr = bp + 4
proc object_string_show
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [object_ptr]
    mov es, ax

    push 0
    push [es:OBJECT_STRING_OFF_SOURCE]
    call print_cstr

    pop es
    pop ax
    pop bp
    ret 2
endp object_string_show

; Returns whether the two strings are equal
lhs_ptr = bp + 4
rhs_ptr = bp + 6
proc object_string_eq
    push bp
    mov bp, sp
    push es

    mov ax, [rhs_ptr]
    mov es, ax
    push [es:OBJECT_STRING_OFF_SOURCE]
    mov ax, [lhs_ptr]
    mov es, ax
    push [es:OBJECT_STRING_OFF_SOURCE]
    call cstrs_eq

    pop es
    pop bp
    ret 4
endp object_string_eq

; Define a chr method/function for the string object
object_ptr = bp + 4
proc object_string_chr
    push bp
    mov bp, sp
    push es

    ; Store the object segment in es
    mov ax, [object_ptr]
    mov es, ax

    ; String must be of length 1
    cmp [word ptr es:OBJECT_STRING_OFF_LENGTH], 1
    jne @@error_string_invalid

    ; Store source segment in es
    mov ax, [es:OBJECT_STRING_OFF_SOURCE]
    mov es, ax

    ; Get first byte and make it a word
    mov al, [es:0]
    cbw
    ; Create a number object from the word
    push ax
    call object_number_new
    jmp @@end_run

@@error_string_invalid:
    push offset error_message_string_not_char
    call runtime_error_no_state

@@end_run:

    pop es
    pop bp
    ret 2
endp object_string_chr

; Convert string to a boolean value (if length > 0, 1 otherwise 0)
object_ptr = bp + 4
proc object_string_to_bool
    push bp
    mov bp, sp
    push es

    mov ax, [object_ptr]
    mov es, ax

    mov ax, [es:OBJECT_STRING_OFF_LENGTH]
    test ax, ax
    jz @@zero_length

    mov ax, 1
    jmp @@end_decide_truthy

@@zero_length:
    mov ax, 0

@@end_decide_truthy:

    pop es
    pop bp
    ret 2
endp object_string_to_bool

char = bp + 4
cstr_ptr = bp - 2
proc object_string_single_char_new
    push bp
    mov bp, sp
    sub sp, 2
    push es

    ; Allocate cstr
    push 1 + 1 ; Byte + NUL
    call heap_alloc
    mov es, ax
    ; Set first char
    mov ax, [char]
    mov [es:0], al ; Add character
    mov [byte ptr es:1], 0 ; Add NUL

    ; Store cstr
    mov ax, es
    mov [cstr_ptr], ax

    ; Create new string object
    push offset object_string_type
    call object_new
    mov es, ax

    ; Set string info
    mov [word ptr es:OBJECT_STRING_OFF_LENGTH], 1
    mov ax, [cstr_ptr]
    mov [es:OBJECT_STRING_OFF_SOURCE], ax

    ; Return string object
    mov ax, es

    pop es
    add sp, 2
    pop bp
    ret 2
endp object_string_single_char_new
