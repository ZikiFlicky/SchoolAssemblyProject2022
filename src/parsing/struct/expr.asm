DATASEG

    ; Expr types
    EXPR_TYPE_NUMBER = 1
    EXPR_TYPE_VAR = 2
    EXPR_TYPE_ADD = 3
    EXPR_TYPE_SUB = 4
    EXPR_TYPE_MUL = 5
    EXPR_TYPE_DIV = 6
    EXPR_TYPE_MOD = 7
    EXPR_TYPE_NEG = 8
    EXPR_TYPE_CMP_EQUALS = 9
    EXPR_TYPE_CMP_NOT_EQUAL = 10
    EXPR_TYPE_CMP_SMALLER = 11
    EXPR_TYPE_CMP_BIGGER = 12
    EXPR_TYPE_CMP_SMALLER_EQUALS = 13
    EXPR_TYPE_CMP_BIGGER_EQUALS = 14
    EXPR_TYPE_AND = 15
    EXPR_TYPE_OR = 16
    EXPR_TYPE_NOT = 17
    EXPR_TYPE_STRING = 18
    EXPR_TYPE_VECTOR = 19
    EXPR_TYPE_CHR = 20

    ; Expr offsets
    EXPR_OFF_TYPE = 0
    EXPR_OFF_FILE_INDEX = 1

    EXPR_NUMBER_OFF_NUMBER = 3

    EXPR_STRING_OFF_SOURCE = 3
    EXPR_STRING_OFF_LENGTH = 5

    EXPR_BINARY_OFF_LHS = 3
    EXPR_BINARY_OFF_RHS = 5

    EXPR_VECTOR_OFF_X = 3
    EXPR_VECTOR_OFF_Y = 5

    EXPR_VAR_OFF_NAME = 3

    EXPR_SINGLE_OFF_INNER = 3

    EXPR_MAX_SIZE = 7

CODESEG
; Returns segment of new expr with the given type
expr_type = bp + 4
index = bp + 6
proc expr_new
    push bp
    mov bp, sp
    push es

    ; Allocate
    push EXPR_MAX_SIZE
    call heap_alloc
    mov es, ax
    ; Add type
    mov ax, [expr_type]
    mov [es:EXPR_OFF_TYPE], al
    mov ax, [index]
    mov [es:EXPR_OFF_FILE_INDEX], ax
    ; Prepare for return
    mov ax, es

    pop es
    pop bp
    ret 4
endp expr_new

; Deletes an expression
expr_ptr = bp + 4
proc expr_delete
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [expr_ptr]
    mov es, ax

    mov al, [es:EXPR_OFF_TYPE]

    cmp al, EXPR_TYPE_VAR
    je @@choice_var
    cmp al, EXPR_TYPE_ADD
    je @@choice_binary
    cmp al, EXPR_TYPE_SUB
    je @@choice_binary
    cmp al, EXPR_TYPE_MUL
    je @@choice_binary
    cmp al, EXPR_TYPE_DIV
    je @@choice_binary
    cmp al, EXPR_TYPE_MOD
    je @@choice_binary
    cmp al, EXPR_TYPE_AND
    je @@choice_binary
    cmp al, EXPR_TYPE_OR
    je @@choice_binary
    cmp al, EXPR_TYPE_CMP_EQUALS
    je @@choice_binary
    cmp al, EXPR_TYPE_CMP_NOT_EQUAL
    je @@choice_binary
    cmp al, EXPR_TYPE_CMP_SMALLER
    je @@choice_binary
    cmp al, EXPR_TYPE_CMP_BIGGER
    je @@choice_binary
    cmp al, EXPR_TYPE_CMP_SMALLER_EQUALS
    je @@choice_binary
    cmp al, EXPR_TYPE_CMP_BIGGER_EQUALS
    je @@choice_binary
    cmp al, EXPR_TYPE_NEG
    je @@choice_single
    cmp al, EXPR_TYPE_CHR
    je @@choice_single
    cmp al, EXPR_TYPE_STRING
    je @@choice_sting

    ; We don't need to remove anything
    jmp @@choice_end

@@choice_var:
    push [es:EXPR_VAR_OFF_NAME]
    call heap_free
    jmp @@choice_end

@@choice_binary:
    push [es:EXPR_BINARY_OFF_LHS]
    call expr_delete
    push [es:EXPR_BINARY_OFF_RHS]
    call expr_delete
    jmp @@choice_end

@@choice_single:
    push [es:EXPR_SINGLE_OFF_INNER]
    call expr_delete
    jmp @@choice_end

@@choice_sting:
    push [es:EXPR_STRING_OFF_SOURCE]
    call heap_free

@@choice_end:

    push es
    call heap_free

    pop es
    pop ax
    pop bp
    ret 2
endp expr_delete
