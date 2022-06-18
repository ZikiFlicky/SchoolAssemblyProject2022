DATASEG
    ; Instruction types
    INSTRUCTION_TYPE_ASSIGN = 1
    INSTRUCTION_TYPE_SHOW = 2
    INSTRUCTION_TYPE_IF = 3
    INSTRUCTION_TYPE_LOOP = 4
    INSTRUCTION_TYPE_XLINE = 5
    INSTRUCTION_TYPE_YLINE = 6
    INSTRUCTION_TYPE_RECT = 7
    INSTRUCTION_TYPE_FILLEDRECT = 8
    INSTRUCTION_TYPE_DIAGONALLINE = 9
    INSTRUCTION_TYPE_SETCOLOR = 10
    INSTRUCTION_TYPE_BLOCK = 11
    INSTRUCTION_TYPE_SETWRITEPOS = 12

    ; Instruction offsets
    INSTRUCTION_OFF_TYPE = 0
    INSTRUCTION_OFF_FILE_INDEX = 1

    INSTRUCTION_ASSIGN_OFF_KEY = 3
    INSTRUCTION_ASSIGN_OFF_EXPR = 5

    INSTRUCTION_ONE_ARG_OFF_ARG = 3

    INSTRUCTION_IF_OFF_EXPR = 3
    INSTRUCTION_IF_OFF_INSTRUCTION = 5
    INSTRUCTION_IF_OFF_ELSE_INSTRUCTION = 7

    INSTRUCTION_LOOP_OFF_EXPR = 3
    INSTRUCTION_LOOP_OFF_INSTRUCTION = 5

    INSTRUCTION_ONE_ARG_OFF_ARG = 3

    INSTRUCTION_TWO_ARGS_OFF_ARG1 = 3
    INSTRUCTION_TWO_ARGS_OFF_ARG2 = 5

    INSTRUCTION_BLOCK_OFF_INSTRUCTIONS = 3
    INSTRUCTION_BLOCK_OFF_LENGTH = 5

    INSTRUCTION_MAX_SIZE = 9

CODESEG
; Returns segment of new instruction with the given type
instruction_type = bp + 4
proc instruction_new
    push bp
    mov bp, sp
    push es

    push INSTRUCTION_MAX_SIZE
    call heap_alloc
    mov es, ax

    mov ax, [instruction_type] ; Only al stores the type
    mov [es:INSTRUCTION_OFF_TYPE], al

    ; To return the instruction
    mov ax, es

    pop es
    pop bp
    ret 2
endp instruction_new

; Delete an assign instruction
instruction_ptr = bp + 4
proc instruction_assign_delete
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    push [es:INSTRUCTION_ASSIGN_OFF_KEY]
    call heap_free
    push [es:INSTRUCTION_ASSIGN_OFF_EXPR]
    call expr_delete

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_assign_delete

; Delete an `if` instruction
instruction_ptr = bp + 4
proc instruction_if_delete
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    push [es:INSTRUCTION_IF_OFF_EXPR]
    call expr_delete
    push [es:INSTRUCTION_IF_OFF_INSTRUCTION]
    call instruction_delete
    cmp [word ptr es:INSTRUCTION_IF_OFF_ELSE_INSTRUCTION], 0
    je @@no_else_block
    ; If we got here we have an else block
    push [es:INSTRUCTION_IF_OFF_ELSE_INSTRUCTION]
    call instruction_delete

@@no_else_block:

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_if_delete

; Delete a `loop` instruction
instruction_ptr = bp + 4
proc instruction_loop_delete
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    push [es:INSTRUCTION_LOOP_OFF_EXPR]
    call expr_delete
    push [es:INSTRUCTION_LOOP_OFF_INSTRUCTION]
    call instruction_delete

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_loop_delete

; Delete a two arg instructions
instruction_ptr = bp + 4
proc instruction_two_args_delete
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    push [es:INSTRUCTION_TWO_ARGS_OFF_ARG1]
    call expr_delete
    push [es:INSTRUCTION_TWO_ARGS_OFF_ARG2]
    call expr_delete

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_two_args_delete

; Delete a one arg instructions
instruction_ptr = bp + 4
proc instruction_one_arg_delete
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    push [es:INSTRUCTION_ONE_ARG_OFF_ARG]
    call expr_delete

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_one_arg_delete

; Delete a block instruction
instruction_ptr = bp + 4
proc instruction_block_delete
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push es

    mov ax, [instruction_ptr]
    mov es, ax
    mov ax, [es:INSTRUCTION_BLOCK_OFF_INSTRUCTIONS]
    mov es, ax
    mov cx, [es:INSTRUCTION_BLOCK_OFF_LENGTH]
    mov ax, [es:INSTRUCTION_BLOCK_OFF_INSTRUCTIONS]
    mov es, ax

    mov bx, 0
@@remove_instruction:
    cmp cx, 0
    je @@end_remove_block

    push [es:bx]
    call instruction_delete

    dec cx
    add bx, 2
    jmp @@remove_instruction

@@end_remove_block:
    push es
    call heap_free

    pop es
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
endp instruction_block_delete

; Deletes an instruction
instruction_ptr = bp + 4
proc instruction_delete
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    mov al, [es:INSTRUCTION_OFF_TYPE]

    ; Remove contents by type
    cmp al, INSTRUCTION_TYPE_ASSIGN
    je @@choice_assign
    cmp al, INSTRUCTION_TYPE_IF
    je @@choice_if
    cmp al, INSTRUCTION_TYPE_LOOP
    je @@choice_loop
    cmp al, INSTRUCTION_TYPE_XLINE
    je @@choice_two_args
    cmp al, INSTRUCTION_TYPE_YLINE
    je @@choice_two_args
    cmp al, INSTRUCTION_TYPE_RECT
    je @@choice_two_args
    cmp al, INSTRUCTION_TYPE_FILLEDRECT
    je @@choice_two_args
    cmp al, INSTRUCTION_TYPE_DIAGONALLINE
    je @@choice_two_args
    cmp al, INSTRUCTION_TYPE_SHOW
    je @@choice_one_arg
    cmp al, INSTRUCTION_TYPE_SETCOLOR
    je @@choice_one_arg
    cmp al, INSTRUCTION_TYPE_BLOCK
    je @@choice_block
    cmp al, INSTRUCTION_TYPE_SETWRITEPOS
    je @@choice_one_arg

    ; If we got here we might not need to delete it
    jmp @@no_delete_func

@@choice_assign:
    mov ax, offset instruction_assign_delete
    jmp @@choice_end

@@choice_if:
    mov ax, offset instruction_if_delete
    jmp @@choice_end

@@choice_loop:
    mov ax, offset instruction_loop_delete
    jmp @@choice_end

@@choice_two_args:
    mov ax, offset instruction_two_args_delete
    jmp @@choice_end

@@choice_one_arg:
    mov ax, offset instruction_one_arg_delete
    jmp @@choice_end

@@choice_block:
    mov ax, offset instruction_block_delete

@@choice_end:

    ; Call the delete function
    push es
    call ax

@@no_delete_func:

    ; Free the address of the whole instruction
    push es
    call heap_free

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_delete
