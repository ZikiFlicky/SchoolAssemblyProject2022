DATASEG
    ; Global token information
    token_type db ?
    token_start_idx dw ?
    token_length dw ?

    ; Token types
    TOKEN_TYPE_VAR = 1
    TOKEN_TYPE_EQU = 2
    TOKEN_TYPE_PLUS = 3
    TOKEN_TYPE_MINUS = 4
    TOKEN_TYPE_STAR = 5
    TOKEN_TYPE_SLASH = 6
    TOKEN_TYPE_PERCENT = 7
    TOKEN_TYPE_NUMBER = 8
    TOKEN_TYPE_NEWLINE = 9
    TOKEN_TYPE_SHOW = 10
    TOKEN_TYPE_IF = 11
    TOKEN_TYPE_ELSE = 12
    TOKEN_TYPE_LOOP = 13
    TOKEN_TYPE_LEFT_BRACE = 14
    TOKEN_TYPE_RIGHT_BRACE = 15
    TOKEN_TYPE_LEFT_PAREN = 16
    TOKEN_TYPE_RIGHT_PAREN = 17
    TOKEN_TYPE_EQU_EQU = 18
    TOKEN_TYPE_EXCLAMATION_MARK_EQU = 19
    TOKEN_TYPE_LESS_EQU = 20
    TOKEN_TYPE_GREATER_EQU = 21
    TOKEN_TYPE_LESS_THAN = 22
    TOKEN_TYPE_GREATER_THAN = 23
    TOKEN_TYPE_AMPERSAND_AMPERSAND = 24
    TOKEN_TYPE_PIPE_PIPE = 25
    TOKEN_TYPE_EXCLAMATION_MARK = 26
    TOKEN_TYPE_COMMA = 27
    TOKEN_TYPE_STRING = 28
    TOKEN_TYPE_XLINE = 29
    TOKEN_TYPE_YLINE = 30
    TOKEN_TYPE_RECT = 31
    TOKEN_TYPE_FILLEDRECT = 32
    TOKEN_TYPE_DIAGONALLINE = 33
    TOKEN_TYPE_SETCOLOR = 34
    TOKEN_TYPE_SETWRITEPOS = 35
    TOKEN_TYPE_CHR = 36

    ; Token offsets
    TOKEN_OFF_TYPE = 0
    TOKEN_OFF_START = 1
    TOKEN_OFF_LENGTH = 3

    TOKEN_SIZE = 5

CODESEG
; Takes a token from a segment and an offset in memory and puts the global token into it
token_seg = bp + 4
token_offset = bp + 6
proc token_copy
    push bp
    mov bp, sp
    push ax
    push bx
    push es

    mov ax, [token_seg]
    mov es, ax

    mov bx, [token_offset]

    ; Copy type
    mov al, [token_type]
    mov [es:(bx + TOKEN_OFF_TYPE)], al

    ; Copy start index
    mov ax, [token_start_idx]
    mov [es:(bx + TOKEN_OFF_START)], ax

    ; Copy length
    mov ax, [token_length]
    mov [es:(bx + TOKEN_OFF_LENGTH)], ax

    pop es
    pop bx
    pop ax
    pop bp
    ret 4
endp token_copy

; Takes a token from a segment and an offset in memory and sets them as the global token
token_seg = bp + 4
token_offset = bp + 6
proc token_set
    push bp
    mov bp, sp
    push ax
    push bx
    push es

    mov ax, [token_seg]
    mov es, ax

    mov bx, [token_offset]

    ; Copy type
    mov al, [es:(bx + TOKEN_OFF_TYPE)]
    mov [token_type], al

    ; Copy start index
    mov ax, [es:(bx + TOKEN_OFF_START)]
    mov [token_start_idx], ax

    ; Copy length
    mov ax, [es:(bx + TOKEN_OFF_LENGTH)]
    mov [token_length], ax

    pop es
    pop bx
    pop ax
    pop bp
    ret 4
endp token_set

; Converts a token into a heap-allocated cstr (NUL-terminated string) and returns it
backtrack = bp - 2
proc token_to_cstr
    push bp
    mov bp, sp
    sub sp, 2
    push bx
    push cx
    push es

    ; Store current index
    mov ax, [file_idx]
    mov [backtrack], ax

    push [token_start_idx]
    call file_set_idx

    ; Read the bytes
    push [token_length]
    call read_bytes

    mov ax, [token_length]
    inc ax ; For NUL
    push ax
    call heap_alloc
    mov es, ax

    mov bx, 0
@@copy_char_to_cstr:
    cmp bx, [token_length]
    je @@end_copy_char_to_cstr

    mov al, [file_read_buffer + bx]
    mov [es:bx], al

    inc bx
    jmp @@copy_char_to_cstr

@@end_copy_char_to_cstr:

    mov [byte ptr es:bx], 0 ; NUL-terminate
    mov ax, es

    push [backtrack]
    call file_set_idx

    pop es
    pop cx
    pop bx
    add sp, 2
    pop bp
    ret
endp token_to_cstr

; Converts a number token into an actual word and returns it
backtrack = bp - 2
number = bp - 4
proc token_to_number
    push bp
    mov bp, sp
    sub sp, 4
    push bx
    push cx
    push dx

    ; Store current index
    mov ax, [file_idx]
    mov [backtrack], ax

    push [token_start_idx]
    call file_set_idx

    ; Read the bytes
    push [token_length]
    call read_bytes

    mov [word ptr number], 0
    mov cx, 0
@@loop_digits:
    mov ax, [number]
    xor dx, dx
    mov bx, 10
    mul bx

    ; Check if the number is larger than 16-bit
    test dx, dx
    jz @@number_in_range

    ; If we got here the number was too big
    push [token_start_idx]
    push offset error_message_number_too_big
    call parser_error

@@number_in_range:

    mov [number], ax ; Move back the number multiplied by 10

    ; Make bx the pointer to the place in which the digit lies
    mov bx, offset file_read_buffer
    add bx, cx

    mov al, [bx]
    sub al, '0'
    cbw
    add [number], ax

    inc cx
    cmp cx, [token_length]
    jb @@loop_digits

    mov ax, [number]

    push [backtrack]
    call file_set_idx

    pop dx
    pop cx
    pop bx
    add sp, 4
    pop bp    ; Copy token contents
    ret
endp token_to_number
