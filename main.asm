IDEAL
MODEL small
STACK 100h
DATASEG
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

    ; Token offsets
    TOKEN_OFF_TYPE = 0
    TOKEN_OFF_START = 1
    TOKEN_OFF_LENGTH = 3

    TOKEN_SIZE = 5

    ; Expr types
    EXPR_TYPE_NUMBER = 1
    EXPR_TYPE_VAR = 2
    EXPR_TYPE_ADD = 3
    EXPR_TYPE_SUB = 4
    EXPR_TYPE_MUL = 5
    EXPR_TYPE_DIV = 6
    EXPR_TYPE_MOD = 7
    EXPR_TYPE_NEG = 8

    ; Expr offsets
    EXPR_OFF_TYPE = 0

    EXPR_NUMBER_OFF_NUMBER = 1

    EXPR_BINARY_OFF_LHS = 1
    EXPR_BINARY_OFF_RHS = 3

    EXPR_VAR_OFF_NAME = 1

    EXPR_SINGLE_OFF_INNER = 1

    EXPR_MAX_SIZE = 5

    ; Instruction types
    INSTRUCTION_TYPE_ASSIGN = 1
    INSTRUCTION_TYPE_SHOW = 2
    INSTRUCTION_TYPE_IF = 3
    INSTRUCTION_TYPE_LOOP = 4

    ; Instruction offsets
    INSTRUCTION_OFF_TYPE = 0

    INSTRUCTION_ASSIGN_OFF_KEY = 1
    INSTRUCTION_ASSIGN_OFF_EXPR = 3

    INSTRUCTION_SHOW_OFF_EXPR = 1

    INSTRUCTION_IF_OFF_EXPR = 1
    INSTRUCTION_IF_OFF_BLOCK = 3
    INSTRUCTION_IF_OFF_ELSE_BLOCK = 5

    INSTRUCTION_LOOP_OFF_EXPR = 1
    INSTRUCTION_LOOP_OFF_BLOCK = 3

    INSTRUCTION_MAX_SIZE = 7

    ; Object types
    OBJECT_TYPE_NUMBER = 1

    ; Object offsets
    OBJECT_OFF_TYPE = 0
    OBJECT_OFF_REFCOUNT = 1

    OBJECT_NUMBER_OFF_NUMBER = 3

    OBJECT_MAX_SIZE = 5

    ; File related
    filename db "CODE.TXT", 0
    file dw ?
    file_idx dw 0
    file_read_buffer db 100h dup(?)

    token_type db ?
    token_start_idx dw ?
    token_length dw ?

    ; Lexer related stuff
    keyword_show db TOKEN_TYPE_SHOW, 4, "show"
    keyword_if db TOKEN_TYPE_IF, 2, "if"
    keyword_else db TOKEN_TYPE_ELSE, 4, "else"
    keyword_loop db TOKEN_TYPE_LOOP, 4, "loop"
    keywords dw offset keyword_show, offset keyword_if, offset keyword_else, offset keyword_loop
    AMOUNT_KEYWORDS = 4

    ; Error related stuff
    file_error_line_message db " [line $"
    file_error_column_message db ", column $"
    file_error_end db "]: $"
    file_error_code_line_start db "> $"

    ; Lexer error related stuff
    lexer_error_start db "LexerError$"
    lexer_error_invalid_token db "Invalid token$"

    ; Parser error related stuff
    parser_error_start db "ParserError$"
    parser_error_invalid_token db "Invalid token$"
    parser_error_syntax_error db "Syntax error$"
    parser_error_expected_newline db "Expected newline$"
    ; Panic related stuff
    panic_message db "* PANIC *", 13, 10, "$"

    ; FIXME: This can overflow + only allows a certain amount of instructions
    parsed_instructions dw 10h dup(?)
    amount_instructions dw 0

    ; Interpreter error stuff
    runtime_error_start db "RuntimeError: $"
    runtime_error_variable_not_found db "Variable not found$"

    ; TODO: Make this a hashmap?
    ; FIXME: Allows up to 16 variables
    ; Interpreter stuff
    variables dw 10h * 2 dup(?)
    amount_variables db 0
CODESEG

; Misc functions

proc panic
    ; Print panic message
    mov ah, 09h
    lea dx, [panic_message]
    int 21h
    mov ah, 4Ch
    mov al, 1 ; Return code 1 = error
    int 21h
    ; We should never reach this code
    ret
endp panic

; Returns into ax whether the given char is a character that can start a variable
character = bp + 4
proc is_char_var_start
    push bp
    mov bp, sp

    ; Load character (only al can possibly have a value different from 0)
    mov ax, [character]

    cmp al, 'a'
    jl not_lowercase
    cmp al, 'z'
    jg not_lowercase

    jmp character_is_var_start

not_lowercase:
    cmp al, 'A'
    jl not_uppercase
    cmp al, 'Z'
    jg not_uppercase

    jmp character_is_var_start

not_uppercase:
    mov ax, 0
    jmp character_var_start_check_end

character_is_var_start:
    mov ax, 1

character_var_start_check_end:

    pop bp
    ret 2
endp is_char_var_start

character = bp + 4
proc is_char_var
    push bp
    mov bp, sp

    push [word ptr character]
    call is_char_var_start
    test ax, ax
    jnz character_is_var_start

    ; Load character (only al can possibly have a value different from 0)
    mov ax, [character]

    cmp al, '0'
    jl not_digit
    cmp al, '9'
    jg not_digit

    mov ax, 1
    jmp character_var_check_end

not_digit:
    mov ax, 0

character_var_check_end:

    pop bp
    ret 2
endp is_char_var

proc print_newline
    push ax
    push bx
    mov ah, 0Eh
    mov al, 13 ; Carriage return
    mov bx, 0
    int 10h
    mov ah, 0Eh
    mov al, 10 ; Line feed
    mov bx, 0
    int 10h
    pop bx
    pop ax
    ret
endp print_newline

string_offset = bp + 4
proc print_nul_terminated_string
    push bp
    mov bp, sp
    push ax
    push bx

    mov bx, [string_offset] ; Start index
print_char_loop:
    ; Get char
    mov al, [bx]
    ; If got terminating NUL, exit loop
    cmp al, 0
    je end_print_char_loop

    push bx ; Because bx is used for the loop
    mov ah, 0Eh
    mov bx, 0
    int 10h
    pop bx

    inc bx
    jmp print_char_loop

end_print_char_loop:

    pop bx
    pop ax
    pop bp
    ret 2
endp print_nul_terminated_string

word_number = bp + 4
proc print_word
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    mov ax, [word_number]

    cmp ax, 0
    jge printed_number_is_positive

    ; Print the negative sign
    mov ah, 0Eh
    mov al, '-'
    mov bx, 0
    int 10h

    ; Flip because we want to print the positive after the '-'
    neg [word ptr word_number]

printed_number_is_positive:
    mov ax, [word_number]
    mov cx, 0
    mov bx, 10

get_word_length:
    inc cx

    xor dx, dx
    div bx

    cmp ax, 0
    jne get_word_length

print_digit:
    ; Divide 10^length times
    mov ax, [word_number]
    mov bx, 10
    ; Divide until you make the result mod 10 equal the digit
    push cx
    dec cx
divide_number:
    cmp cx, 0
    je end_divide_number

    xor dx, dx
    div bx

    dec cx
    jmp divide_number
end_divide_number:
    pop cx
    ; Make the remainder the digit
    xor dx, dx
    div bx
    mov ax, dx

    ; Make the digit an ascii digit
    add al, '0'
    ; Print the negative
    mov ah, 0Eh
    mov bx, 0
    int 10h

    loop print_digit

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
endp print_word

amount = bp + 4
proc heap_alloc
    push bp
    mov bp, sp
    push bx

    mov ax, [amount]
    mov bl, 10h
    div bl
    test ah, ah
    jz exact_fit
    inc al

exact_fit:
    cbw
    mov bx, ax
    mov ah, 48h
    int 21h
    jnc allocation_success

    ; If we got here, we had an allocation failure
    mov ax, 0

allocation_success:

    pop bx
    pop bp
    ret 2
endp heap_alloc

address = bp + 4
proc heap_free
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [address]
    mov es, ax
    mov ah, 49h
    int 21h

    pop es
    pop ax
    pop bp
    ret 2
endp heap_free

heapstr1 = bp + 4
heapstr2 = bp + 6
index = bp - 2
proc cstrs_eq
    push bp
    mov bp, sp
    sub sp, 2
    push bx
    push cx
    push es

    mov [word ptr index], 0

loop_cstrs:
    mov bx, [index]
    ; Get char from first string
    mov ax, [heapstr1]
    mov es, ax
    mov cl, [es:bx]
    ; Get char from second string
    mov ax, [heapstr2]
    mov es, ax
    mov ch, [es:bx]

    cmp cl, ch
    jne cstrs_not_equal

    ; If the characters are NULs
    cmp cl, 0
    jne cstrs_reloop

    ; If we got here, we matched (characters were all the same and NUL is in the same index)
    mov ax, 1
    jmp end_loop_cstrs

cstrs_reloop:

    inc [word ptr index]
    jmp loop_cstrs

cstrs_not_equal:
    mov ax, 0

end_loop_cstrs:

    pop es
    pop cx
    pop bx
    add sp, 2
    pop bp
    ret 4
endp cstrs_eq

; Shows an error
error_start_ptr = bp + 4
message_ptr = bp + 6
index = bp + 8
line = bp - 2
column = bp - 4
backtrack = bp - 6
proc show_file_error
    push bp
    mov bp, sp
    sub sp, 6
    push ax
    push bx
    push cx
    push dx

    ; Store backtrack
    mov ax, [file_idx]
    mov [backtrack], ax
    ; Remove following whitespace and store the index after the whitespace
    push [index]
    call file_set_idx
    call remove_whitespace
    mov ax, [file_idx]
    mov [index], ax
    ; Go back to original position
    push [backtrack]
    call file_set_idx

    mov ah, 09h
    mov dx, [error_start_ptr]
    int 21h

    mov ah, 09h
    lea dx, [byte ptr file_error_line_message]
    int 21h

    push [index]
    call file_get_line_col_of_index
    mov [line], ax
    mov [column], bx

    push [line]
    call print_word

    mov ah, 09h
    lea dx, [byte ptr file_error_column_message]
    int 21h

    push [column]
    call print_word

    mov ah, 09h
    lea dx, [byte ptr file_error_end]
    int 21h

    ; Some error message
    mov ah, 09h
    mov dx, [message_ptr]
    int 21h
    call print_newline

    mov ah, 09h
    lea dx, [byte ptr file_error_code_line_start]
    int 21h

    ; Print rest of line
    mov ax, [index]
    mov bx, [column]
    dec bx
    sub ax, bx
    push ax
    call print_line_from_index

    call print_newline

    pop dx
    pop cx
    pop bx
    pop ax
    add sp, 6
    pop bp
    ret 6
endp show_file_error

index = bp + 4
backtrack = bp - 2
proc print_line_from_index
    push bp
    mov bp, sp
    sub sp, 2
    push ax
    push bx

    mov ax, [file_idx]
    mov [backtrack], ax

    push [index]
    call file_set_idx

@@read_line:
    call file_read_newline
    test ax, ax
    jnz @@end_read_line
    push 1
    call read_bytes
    test ax, ax
    jz @@end_read_line

    ; Put the character
    mov ah, 0Eh
    mov al, [file_read_buffer + 0]
    mov bx, 0
    int 10h

    jmp @@read_line

@@end_read_line:

    ; Reload
    push [backtrack]
    call file_set_idx

    pop bx
    pop ax
    add sp, 2
    pop bp
    ret 2
endp print_line_from_index

; File procedures

proc open_file
    push ax
    push dx

    mov ah, 3Dh
    mov al, 0 ; readonly
    lea dx, [word ptr filename]
    int 21h

    ; TODO: check errors
    mov [file], ax

    pop dx
    pop ax
    ret
endp open_file

proc close_file
    push ax

    ; Interrupt for closing files
    mov ah, 3Eh
    mov bx, [file]
    int 21h

    pop ax 
    ret
endp close_file

idx = bp + 4
proc file_set_idx
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    mov ah, 42h
    mov al, 0 ; Set (not relative)
    mov bx, [file]
    xor cx, cx
    mov dx, [idx]
    int 21h
    ; Move the new index into the file index variable
    mov ax, [idx]
    mov [file_idx], ax

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
endp file_set_idx

amount_bytes = bp + 4
backtrack = bp - 2
proc read_bytes
    push bp
    mov bp, sp
    sub sp, 2
    push bx
    push cx
    push dx

    mov ax, [file_idx]
    mov [backtrack], ax

    mov ah, 3Fh
    mov bx, [file]
    mov cx, [amount_bytes]
    lea dx, [word ptr file_read_buffer]
    int 21h

    ; If had error
    jc had_error

    ; if read less than wanted
    cmp ax, [amount_bytes]
    jl had_error

    mov ax, [amount_bytes]
    add [file_idx], ax

    mov ax, 1
    jmp no_error

had_error:
    push [backtrack]
    call file_set_idx
    mov ax, 0
no_error:

    pop dx
    pop cx
    pop bx
    add sp, 2
    pop bp
    ret 2
endp read_bytes

; FIXME: Had "mov [token_type], TOKEN_TYPE_NEWLINE" which shouldn't be allowed

backtrack = bp - 2
proc file_read_newline
    push bp
    mov bp, sp
    sub sp, 2

    ; Store backtrack
    mov ax, [file_idx]
    mov [backtrack], ax

    push 2
    call read_bytes
    test ax, ax
    jz @@not_found_newline ; You have to have 2 bytes for this

    cmp [byte ptr file_read_buffer], 13
    jne @@maybe_starts_with_linefeed
    cmp [byte ptr file_read_buffer + 1], 10
    jne @@not_found_newline

    jmp @@had_newline

@@maybe_starts_with_linefeed:
    cmp [byte ptr file_read_buffer], 10
    jne @@not_found_newline
    cmp [byte ptr file_read_buffer + 1], 13
    jne @@not_found_newline

    jmp @@had_newline

@@not_found_newline:
    push [backtrack]
    call file_set_idx

    mov ax, 0
    jmp @@finish_newline_lex

@@had_newline:
    mov ax, 1

@@finish_newline_lex:

    add sp, 2
    pop bp
    ret
endp file_read_newline

index = bp + 4
backtrack = bp - 2
line = bp - 4
column = bp - 6
proc file_get_line_col_of_index
    push bp
    mov bp, sp
    sub sp, 6
    push cx

    mov ax, [file_idx]
    mov [backtrack], ax

    ; Set to beggining
    push 0
    call file_set_idx

    ; Set info
    mov [word ptr line], 1
    mov [word ptr column], 1

    mov cx, 0
@@read_file:
    cmp cx, [index]
    jge @@end_read_file
    call file_read_newline
    test ax, ax
    jnz @@read_newline
    push 1
    call read_bytes
    test ax, ax
    jnz @@read_character

    ; We get here when index is bigger than the file (which is impossible logically)
    call panic

@@read_newline:
    inc [word ptr line]
    mov [word ptr column], 1
    add cx, 2
    jmp @@read_file

@@read_character:
    inc [word ptr column]
    inc cx
    jmp @@read_file

@@end_read_file:

    mov ax, [line]
    mov bx, [column]

    pop cx
    add sp, 6
    pop bp
    ret 2
endp file_get_line_col_of_index

; Token procedures

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
copy_char_to_cstr:
    cmp bx, [token_length]
    je end_copy_char_to_cstr

    mov al, [file_read_buffer + bx]
    mov [es:bx], al

    inc bx
    jmp copy_char_to_cstr

end_copy_char_to_cstr:

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
loop_digits:
    mov ax, [number]
    xor dx, dx
    mov bx, 10
    mul bx
    ; FIXME: Check here if we have something in dx, which means that we have a number too big (more than 16-bit)
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
    jl loop_digits

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

; Lexer procedures

; Returns into ax a bool sayng if we were able to lex the newline
times_newline = bp - 2
proc lex_newline
    push bp
    mov bp, sp
    sub sp, 2

    mov [byte ptr token_type], TOKEN_TYPE_NEWLINE
    mov ax, [file_idx]
    mov [token_start_idx], ax ; The file index
    mov [word ptr token_length], 0

    mov [word ptr times_newline], 0

@@find_newline:
    call file_read_newline
    test ax, ax
    jz @@not_found_newline

    add [word ptr token_length], 2
    call remove_whitespace

    inc [word ptr times_newline]
    jmp @@find_newline

@@not_found_newline:
    cmp [word ptr times_newline], 0
    jg @@had_newlines

    mov ax, 0
    jmp @@finish_newline_lex

@@had_newlines:
    mov ax, 1

@@finish_newline_lex:

    add sp, 2
    pop bp
    ret
endp lex_newline

; Returns into ax a bool saying if we were able to lex the number
backtrack = bp - 2
proc lex_number
    push bp
    mov bp, sp
    sub sp, 2
    push bx

    mov ax, [file_idx]
    mov [backtrack], ax

    push 1
    call read_bytes
    test ax, ax
    jz not_number

    mov al, [byte ptr file_read_buffer]
    cmp al, '0'
    jl not_number
    cmp al, '9'
    jg not_number

    mov [token_type], TOKEN_TYPE_NUMBER
    mov ax, [backtrack]
    mov [token_start_idx], ax
    mov [word ptr token_length], 0

still_number:
    inc [word ptr token_length]

    push 1
    call read_bytes
    test ax, ax
    jz end_number

    mov bl, [byte ptr file_read_buffer]
    cmp bl, '0'
    jl end_number_go_prev
    cmp bl, '9'
    jg end_number_go_prev
    jmp still_number

end_number_go_prev:
    ; If we got here there was a non-number character (but there WAS a character), so we must go back 1 step to "unadvance" the character
    mov bx, [word ptr file_idx] ; because we stepped over a non-digit
    dec bx
    push bx
    call file_set_idx

end_number:
    mov ax, 1
    jmp end_number_lex

not_number:
    push [backtrack]
    call file_set_idx
    mov ax, 0
    jmp end_number_lex

end_number_lex:

    pop bx
    add sp, 2
    pop bp
    ret
endp lex_number

keyword_ptr = bp + 4
backtrack = bp - 2
keyword_length = bp - 4
keyword_token_type = bp - 5
keyword_start_ptr = bp - 7
proc lex_single_keyword
    push bp
    mov bp, sp
    sub sp, 7
    push bx
    push cx

    mov ax, [file_idx]
    mov [backtrack], ax

    mov bx, [keyword_ptr]
    mov al, [bx + 0]
    mov [keyword_token_type], al
    lea ax, [bx + 2]
    mov [keyword_start_ptr], ax
    mov al, [bx + 1]
    cbw
    mov [word ptr keyword_length], ax

    inc ax
    push ax
    call read_bytes
    test ax, ax
    jz @@read_less

    ; If we got here, we need to check if the last character is a keyword char or not
    lea bx, [file_read_buffer]
    add bx, [keyword_length]
    mov al, [bx]
    cbw
    push ax
    call is_char_var ; Check if keyword char
    test ax, ax
    jnz @@lex_failed

    ; Used to read just enough bytes if the last byte was not a keyword character or if there is no additional character (EOF)
@@read_less:
    ; Restore previous state
    push [backtrack]
    call file_set_idx

    push [keyword_length]
    call read_bytes
    test ax, ax
    jz @@lex_failed

    mov cx, 0
@@keyword_char_cmp:
    mov bx, [keyword_start_ptr]
    add bx, cx
    mov al, [bx]

    lea bx, [file_read_buffer]
    add bx, cx
    mov ah, [bx]

    cmp al, ah
    jne @@lex_failed

    inc cx
    cmp cx, [keyword_length]
    jl @@keyword_char_cmp

    ; If we got here, we succeeded
    mov al, [keyword_token_type]
    mov [token_type], al
    mov ax, [keyword_length]
    mov [token_length], ax
    mov ax, [backtrack]
    mov [token_start_idx], ax

    mov ax, 1
    jmp @@lex_end

@@lex_failed:
    push [backtrack]
    call file_set_idx

    mov ax, 0

@@lex_end:

    pop cx
    pop bx
    add sp, 7
    pop bp
    ret 2
endp lex_single_keyword

proc lex_keywords
    push bp
    mov bp, sp
    push bx
    push cx

    mov cx, 0
    lea bx, [keywords]

@@loop_keywords:
    cmp cx, AMOUNT_KEYWORDS
    je @@failed_finding_keyword

    push [bx]
    call lex_single_keyword
    test ax, ax
    jnz @@succeeded_finding_keyword

    inc cx
    add bx, 2
    jmp @@loop_keywords

@@succeeded_finding_keyword:
    mov ax, 1
    jmp @@end_keyword_match

@@failed_finding_keyword:
    mov ax, 0

@@end_keyword_match:

    pop cx
    pop bx
    pop bp
    ret
endp lex_keywords

; Returns into ax a bool saying if we were able to lex the variable name
backtrack = bp - 2
proc lex_var
    push bp
    mov bp, sp
    sub sp, 2

    mov ax, [file_idx]
    mov [backtrack], ax

    push 1
    call read_bytes
    test ax, ax
    jz not_var

    mov al, [file_read_buffer]
    cbw
    push ax
    call is_char_var_start
    test ax, ax
    jz not_var

    mov [token_type], TOKEN_TYPE_VAR
    mov ax, [backtrack]
    mov [token_start_idx], ax
    mov [word ptr token_length], 0

still_var:
    inc [word ptr token_length]

    push 1
    call read_bytes
    test ax, ax
    jz end_var

    mov al, [file_read_buffer]
    cbw
    push ax
    call is_char_var
    test ax, ax
    jz end_var_go_prev
    jmp still_var

end_var_go_prev:
    ; If we got here there was a non-var character (but there WAS a character), so we must go back 1 step to "unadvance" the character
    mov ax, [word ptr file_idx] ; because we stepped over a non-digit
    dec ax
    push ax
    call file_set_idx

end_var:
    mov ax, 1
    jmp end_var_lex

not_var:
    push [backtrack]
    call file_set_idx
    mov ax, 0
    jmp end_var_lex

end_var_lex:

    add sp, 2
    pop bp
    ret
endp lex_var

new_token_type = bp + 5
character = bp + 4
backtrack = bp - 2
proc lex_char
    push bp
    mov bp, sp
    sub sp, 2

    mov ax, [file_idx]
    mov [backtrack], ax

    push 1
    call read_bytes
    test ax, ax
    jz lex_char_fail

    mov al, [file_read_buffer]
    cmp al, [character]
    jne lex_char_fail

    ; Set token
    mov al, [new_token_type]
    mov [token_type], al
    mov ax, [backtrack]
    mov [token_start_idx], ax
    mov [token_length], 1
    mov ax, 1
    jmp lex_char_end

lex_char_fail:
    push [backtrack]
    call file_set_idx
    mov ax, 0
lex_char_end:

    add sp, 2
    pop bp
    ret 2
endp lex_char

message_ptr = bp + 4
index = bp + 6
proc lexer_error
    push bp
    mov bp, sp
    push ax

    push [index]
    push [message_ptr]
    push offset lexer_error_start
    call show_file_error

    ; FIXME: We shouldn't exit here, but this is our only way now
    mov ah, 4Ch
    mov al, 1
    int 21h

    pop ax
    pop bp
    ret 2
endp lexer_error

; Returns into ax whether we found a comment
backtrack = bp - 2
proc remove_comment
    push bp
    mov bp, sp
    sub sp, 2

    mov ax, [file_idx]
    mov [backtrack], ax
    push 1
    call read_bytes

    cmp [byte ptr file_read_buffer + 0], '#'
    je @@loop_comment

    ; If we got here, we didn't find a comment
    push [backtrack]
    call file_set_idx
    mov ax, 0
    jmp @@end_comment_remove

@@loop_comment:
    mov ax, [file_idx]
    mov [backtrack], ax
    ; If found newline, we can end the comment
    call file_read_newline
    test ax, ax
    jnz @@end_comment
    push 1
    ; If we got to the end of the file, we can end the comment, otherwise do nothing
    call read_bytes
    test ax, ax
    jz @@end_comment

    jmp @@loop_comment

@@end_comment:
    ; To un-advance the newline
    push [backtrack]
    call file_set_idx
    mov ax, 1

@@end_comment_remove:

    add sp, 2
    pop bp
    ret
endp remove_comment

backtrack = bp - 2
proc remove_whitespace
    push bp
    mov bp, sp
    sub sp, 2
    push ax

keep_removing:
    mov ax, [file_idx]
    mov [backtrack], ax
    call remove_comment
    test ax, ax
    jnz keep_removing
    push 1
    call read_bytes
    test ax, ax
    jz end_removing
    cmp [byte ptr file_read_buffer], ' '
    je keep_removing
    cmp [byte ptr file_read_buffer], 9 ; Tab
    je keep_removing
    push [backtrack]
    call file_set_idx
end_removing:

    pop ax
    add sp, 2
    pop bp
    ret
endp remove_whitespace

backtrack = bp - 2
proc lex
    push bp
    mov bp, sp
    sub sp, 2

    call remove_whitespace

    mov ax, [file_idx]
    mov [backtrack], ax

    ; Check for EOF
    push 1
    call read_bytes
    push [backtrack]
    call file_set_idx
    test ax, ax
    jnz @@not_eof

    jmp @@lex_failed

@@not_eof:

    call lex_newline
    test ax, ax
    jz @@not_newline

    jmp @@end_lex

@@not_newline:

    call lex_number
    test ax, ax
    jnz @@end_lex

    call lex_keywords
    test ax, ax
    jnz @@end_lex

    call lex_var
    test ax, ax
    jnz @@end_lex

    mov ah, TOKEN_TYPE_EQU
    mov al, '='
    push ax
    call lex_char
    test ax, ax
    jnz @@end_lex

    mov ah, TOKEN_TYPE_PLUS
    mov al, '+'
    push ax
    call lex_char
    test ax, ax
    jnz @@end_lex

    mov ah, TOKEN_TYPE_MINUS
    mov al, '-'
    push ax
    call lex_char
    test ax, ax
    jnz @@end_lex

    mov ah, TOKEN_TYPE_STAR
    mov al, '*'
    push ax
    call lex_char
    test ax, ax
    jnz @@end_lex

    mov ah, TOKEN_TYPE_SLASH
    mov al, '/'
    push ax
    call lex_char
    test ax, ax
    jnz @@end_lex

    mov ah, TOKEN_TYPE_PERCENT
    mov al, '%'
    push ax
    call lex_char
    test ax, ax
    jnz @@end_lex

    mov ah, TOKEN_TYPE_LEFT_BRACE
    mov al, '{'
    push ax
    call lex_char
    test ax, ax
    jnz @@end_lex

    mov ah, TOKEN_TYPE_RIGHT_BRACE
    mov al, '}'
    push ax
    call lex_char
    test ax, ax
    jnz @@end_lex

    push offset lexer_error_invalid_token
    call lexer_error

@@lex_failed:
    mov ax, 0

@@end_lex:
    add sp, 2
    pop bp
    ret
endp lex

; Parser procedures

; Returns segment of new expr with the given type
expr_type = bp + 4
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
    ; Prepare for return
    mov ax, es

    pop es
    pop bp
    ret 2
endp expr_new

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
    cmp al, EXPR_TYPE_NEG
    je @@choice_single

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

@@choice_end:

    push es
    call heap_free

    pop es
    pop ax
    pop bp
    ret 2
endp expr_delete

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
    cmp al, INSTRUCTION_TYPE_SHOW
    je @@choice_show
    cmp al, INSTRUCTION_TYPE_IF
    je @@choice_if
    cmp al, INSTRUCTION_TYPE_LOOP
    je @@choice_loop

    ; If we got here we might not need to delete it
    jmp @@choice_end

@@choice_assign:
    push [es:INSTRUCTION_ASSIGN_OFF_KEY]
    call heap_free
    push [es:INSTRUCTION_ASSIGN_OFF_EXPR]
    call expr_delete
    jmp @@choice_end

@@choice_show:
    push [es:INSTRUCTION_SHOW_OFF_EXPR]
    call expr_delete
    jmp @@choice_end

@@choice_if:
    push [es:INSTRUCTION_IF_OFF_EXPR]
    call expr_delete
    push [es:INSTRUCTION_IF_OFF_BLOCK]
    call block_delete
    cmp [word ptr es:INSTRUCTION_IF_OFF_ELSE_BLOCK], 0
    je @@if_no_else_block
    ; If we got here we have an else block
    push [es:INSTRUCTION_IF_OFF_ELSE_BLOCK]
    call block_delete

@@if_no_else_block:
    jmp @@choice_end

@@choice_loop:
    push [es:INSTRUCTION_LOOP_OFF_EXPR]
    call expr_delete
    push [es:INSTRUCTION_LOOP_OFF_BLOCK]
    call block_delete

@@choice_end:

    ; Free the address of the whole instruction
    push es
    call heap_free

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_delete

message_ptr = bp + 4
index = bp + 6
proc parser_error
    push bp
    mov bp, sp
    push ax

    push [index]
    push [message_ptr]
    push offset parser_error_start
    call show_file_error

    ; FIXME: We shouldn't exit here, but this is our only way now
    mov ah, 4Ch
    mov al, 1
    int 21h

    pop ax
    pop bp
    ret 4
endp parser_error

; Returns into ax whether we matched a token type
expected_type = bp + 4
backtrack = bp - 2
proc parser_match
    push bp
    mov bp, sp
    sub sp, 2

    mov ax, [file_idx]
    mov [backtrack], ax

    call lex
    test ax, ax
    jz not_matched

    ; Load expected token type into al and parsed type into ah
    mov ax, [expected_type]
    mov ah, [token_type]
    cmp al, ah
    jne not_matched

    ; If we got here we do match
    mov ax, 1
    jmp end_match

not_matched:
    push [backtrack]
    call file_set_idx
    mov ax, 0

end_match:

    add sp, 2
    pop bp
    ret 2
endp parser_match

should_error = bp + 4
backtrack = bp - 2
proc parser_expect_newline
    push bp
    mov bp, sp
    sub sp, 2 ; Allocate space for backtrack

    ; Store backtrack
    mov ax, [file_idx]
    mov [backtrack], ax

    call lex
    test ax, ax
    jz @@newline_reached ; If we couldn't lex, this is the end of the file

    cmp [byte ptr token_type], TOKEN_TYPE_NEWLINE
    je @@newline_reached

    ; If we got here, we should backtrack to the beggining of the token
    push [backtrack]
    call file_set_idx

    ; If we shouldn't error we should return false
    mov ax, [should_error]
    test ax, ax
    jz @@not_newline_reached

    ; Error otherwise
    push [backtrack]
    push offset parser_error_expected_newline
    call parser_error

@@newline_reached:
    mov ax, 0
    jmp @@end_expect_newline

@@not_newline_reached:
    mov ax, 1

@@end_expect_newline:

    add sp, 2
    pop bp
    ret 2
endp parser_expect_newline

number = bp - 2
proc parser_parse_expr_number
    push bp
    mov bp, sp
    sub sp, 2
    push es

    ; Try to match a number token
    push TOKEN_TYPE_NUMBER
    call parser_match
    test ax, ax
    jz parse_expr_number_finish

    call token_to_number ; Moves the actual number into ax
    mov [number], ax

    push EXPR_TYPE_NUMBER
    call expr_new
    mov es, ax

    mov ax, [number]
    mov [es:EXPR_NUMBER_OFF_NUMBER], ax

    ; Move the address of the expr to ax so we can return it
    mov ax, es
    jmp parse_expr_number_finish

parse_expr_number_failed:
    mov ax, 0

parse_expr_number_finish:

    pop es
    add sp, 2
    pop bp
    ret
endp parser_parse_expr_number

var_name = bp - 2
proc parser_parse_expr_var    
    push bp
    mov bp, sp
    sub sp, 2
    push es

    push TOKEN_TYPE_VAR
    call parser_match
    test ax, ax
    jz not_found_expr_var

    call token_to_cstr
    mov [var_name], ax

    push EXPR_TYPE_VAR
    call expr_new
    mov es, ax

    mov ax, [var_name]
    mov [es:EXPR_VAR_OFF_NAME], ax

    mov ax, es
    jmp end_parse_expr_var

not_found_expr_var:
    mov ax, 0

end_parse_expr_var:

    pop es
    add sp, 2
    pop bp
    ret
endp parser_parse_expr_var

inner_expr_ptr = bp - 2
proc parser_parse_expr_neg
    push bp
    mov bp, sp
    sub sp, 2
    push es

    push TOKEN_TYPE_MINUS
    call parser_match
    test ax, ax
    jnz @@matched_minus

    mov ax, 0
    jmp @@end_parse_neg

@@matched_minus:
    call parser_parse_expr_single
    test ax, ax
    jz @@error_expected_expr

    ; If we got here we parsed an expression
    mov [inner_expr_ptr], ax
    push EXPR_TYPE_NEG
    call expr_new
    mov es, ax
    mov ax, [inner_expr_ptr]
    mov [es:EXPR_SINGLE_OFF_INNER], ax
    mov ax, es
    jmp @@end_parse_neg

@@error_expected_expr:
    push [file_idx]
    push offset parser_error_syntax_error
    call parser_error

@@end_parse_neg:

    pop es
    add sp, 2
    pop bp
    ret
endp parser_parse_expr_neg

; Parse values/variables
proc parser_parse_expr_single
    call parser_parse_expr_neg
    test ax, ax
    jnz @@end_parse
    call parser_parse_expr_var
    test ax, ax
    jnz @@end_parse
    call parser_parse_expr_number
    test ax, ax
    jnz @@end_parse

    ; If we got here, nothing was parsed
    mov ax, 0

@@end_parse:

    ret
endp parser_parse_expr_single

left_ptr = bp - 2
right_ptr = bp - 4
new_expr_type = bp - 6
proc parser_parse_expr_product
    push bp
    mov bp, sp
    sub sp, 6
    push es

    call parser_parse_expr_single
    mov [left_ptr], ax
    test ax, ax
    jnz @@parse_expr_loop

    jmp @@parse_expr_failed

@@parse_expr_loop:
    mov [word ptr new_expr_type], EXPR_TYPE_MUL
    push TOKEN_TYPE_STAR
    call parser_match
    test ax, ax
    jnz @@matched
    mov [word ptr new_expr_type], EXPR_TYPE_DIV
    push TOKEN_TYPE_SLASH
    call parser_match
    test ax, ax
    jnz @@matched
    mov [word ptr new_expr_type], EXPR_TYPE_MOD
    push TOKEN_TYPE_PERCENT
    call parser_match
    test ax, ax
    jnz @@matched

    mov ax, [left_ptr]
    jmp @@parse_expr_finish

@@matched:
    ; Try to parse the expr after the operator
    call parser_parse_expr_single
    test ax, ax
    jz @@error_expected_expr
    mov [right_ptr], ax

    ; Create a new expr and put the information into it
    push [new_expr_type]
    call expr_new
    mov es, ax
    mov ax, [left_ptr]
    mov [es:EXPR_BINARY_OFF_LHS], ax
    mov ax, [right_ptr]
    mov [es:EXPR_BINARY_OFF_RHS], ax
    mov [left_ptr], es

    jmp @@parse_expr_loop

@@error_expected_expr:
    push [file_idx]
    push offset parser_error_syntax_error
    call parser_error

@@parse_expr_failed:
    mov ax, 0

@@parse_expr_finish:

    pop es
    add sp, 6
    pop bp
    ret
endp parser_parse_expr_product

left_ptr = bp - 2
right_ptr = bp - 4
new_expr_type = bp - 6
proc parser_parse_expr_sum
    push bp
    mov bp, sp
    sub sp, 6
    push es

    call parser_parse_expr_product
    mov [left_ptr], ax
    test ax, ax
    jnz @@parse_expr_loop

    jmp @@parse_expr_failed

@@parse_expr_loop:
    mov [word ptr new_expr_type], EXPR_TYPE_ADD
    push TOKEN_TYPE_PLUS
    call parser_match
    test ax, ax
    jnz @@matched
    mov [word ptr new_expr_type], EXPR_TYPE_SUB
    push TOKEN_TYPE_MINUS
    call parser_match
    test ax, ax
    jnz @@matched

    mov ax, [left_ptr]
    jmp @@parse_expr_finish

@@matched:
    ; Try to parse the expr after the operator
    call parser_parse_expr_product
    test ax, ax
    jz @@error_expected_expr
    mov [right_ptr], ax

    ; Create a new expr and put the information into it
    push [new_expr_type]
    call expr_new
    mov es, ax
    mov ax, [left_ptr]
    mov [es:EXPR_BINARY_OFF_LHS], ax
    mov ax, [right_ptr]
    mov [es:EXPR_BINARY_OFF_RHS], ax
    mov [left_ptr], es

    jmp @@parse_expr_loop

@@error_expected_expr:
    push [file_idx]
    push offset parser_error_syntax_error
    call parser_error

@@parse_expr_failed:
    mov ax, 0

@@parse_expr_finish:

    pop es
    add sp, 6
    pop bp
    ret
endp parser_parse_expr_sum

; The upmost expression parser
proc parser_parse_expr
    call parser_parse_expr_sum
    ret
endp parser_parse_expr

key_token = bp - TOKEN_SIZE
expr_ptr = bp - TOKEN_SIZE - 2
key_segment = bp - TOKEN_SIZE - 2 - 2
proc parser_parse_assignment
    push bp
    mov bp, sp
    sub sp, TOKEN_SIZE + 2 + 2
    push es

    push TOKEN_TYPE_VAR
    call parser_match
    test ax, ax
    jnz assignment_var_found

    jmp not_parse_assignment

assignment_var_found:

    ; Copy current token into key_token
    lea ax, [key_token]
    push ax
    push ss
    call token_copy

    push TOKEN_TYPE_EQU
    call parser_match
    test ax, ax
    jz @@error_no_equal_sign

    call parser_parse_expr
    test ax, ax
    jz @@error_no_value
    mov [expr_ptr], ax ; Store expr

    ; Set the global token to the key token
    lea ax, [key_token]
    push ax
    push ss
    call token_set
    ; Convert to cstr (NUL-terminated string)
    call token_to_cstr
    mov [key_segment], ax

    ; Create new instruction with type Assign
    push INSTRUCTION_TYPE_ASSIGN
    call instruction_new

    ; Make es point to the new instruction segment
    mov es, ax
    ; Store the key in the instruction
    mov ax, [key_segment]
    mov [es:INSTRUCTION_ASSIGN_OFF_KEY], ax
    ; Store the number in the instruction
    mov ax, [expr_ptr]
    mov [es:INSTRUCTION_ASSIGN_OFF_EXPR], ax

    ; Move es back to ax so we can return the pointer to the instruction
    mov ax, es

    jmp end_parse_assignment

@@error_no_equal_sign:
@@error_no_value:
    push [file_idx]
    push offset parser_error_invalid_token
    call parser_error
    jmp end_parse_assignment

not_parse_assignment:
    mov ax, 0

end_parse_assignment:

    pop es
    add sp, TOKEN_SIZE + 2 + 2
    pop bp
    ret
endp parser_parse_assignment

; Parse the SHOW instruction
expr_ptr = bp - 2
proc parser_parse_show
    push bp
    mov bp, sp
    sub sp, 2
    push es

    push TOKEN_TYPE_SHOW
    call parser_match
    test ax, ax
    jz not_parse_show

    call parser_parse_expr
    test ax, ax
    jz show_error_no_value
    mov [expr_ptr], ax

    push INSTRUCTION_TYPE_SHOW
    call instruction_new
    mov es, ax

    mov ax, [expr_ptr]
    mov [es:INSTRUCTION_SHOW_OFF_EXPR], ax

    mov ax, es
    jmp end_parse_show

show_error_no_value:
    push [file_idx]
    push offset parser_error_invalid_token
    call parser_error
    jmp end_parse_show

not_parse_show:
    mov ax, 0

end_parse_show:

    pop es
    add sp, 2
    pop bp
    ret
endp parser_parse_show

; Parse the IF instruction
expr_ptr = bp - 2
block_ptr = bp - 4
else_block_ptr = bp - 6
proc parser_parse_if
    push bp
    mov bp, sp
    sub sp, 6
    push es

    push TOKEN_TYPE_IF
    call parser_match
    test ax, ax
    jnz @@start_parse

    ; If not matched an if-statement
    jmp @@not_parse_if

@@start_parse:

    call parser_parse_expr
    test ax, ax
    jz @@if_error_no_value
    mov [expr_ptr], ax

    call parser_parse_block
    test ax, ax
    jz @@if_error_no_block
    mov [block_ptr], ax

    ; See if we have an else
    push TOKEN_TYPE_ELSE
    call parser_match
    test ax, ax
    jz @@not_found_else

    call parser_parse_block
    test ax, ax
    jz @@not_found_else_block
    mov [else_block_ptr], ax

    jmp @@create_if_instruction

@@not_found_else:
    mov [word ptr else_block_ptr], 0

@@create_if_instruction:

    push INSTRUCTION_TYPE_IF
    call instruction_new
    mov es, ax

    mov ax, [expr_ptr]
    mov [es:INSTRUCTION_IF_OFF_EXPR], ax

    mov ax, [block_ptr]
    mov [es:INSTRUCTION_IF_OFF_BLOCK], ax

    mov ax, [else_block_ptr]
    mov [es:INSTRUCTION_IF_OFF_ELSE_BLOCK], ax

    mov ax, es
    jmp @@end_parse

@@if_error_no_value:
    push [file_idx]
    push offset parser_error_invalid_token
    call parser_error
    jmp @@end_parse

@@not_found_else_block:
@@if_error_no_block:
    push [file_idx]
    push offset parser_error_syntax_error
    call parser_error
    jmp @@end_parse

@@not_parse_if:
    mov ax, 0
    jmp @@end_parse

@@end_parse:

    pop es
    add sp, 6
    pop bp
    ret
endp parser_parse_if

; Parse the loop instruction
expr_ptr = bp - 2
block_ptr = bp - 4
proc parser_parse_loop
    push bp
    mov bp, sp
    sub sp, 4
    push es

    push TOKEN_TYPE_LOOP
    call parser_match
    test ax, ax
    jnz @@start_parse

    ; If not matched a loop-statement
    jmp @@not_parse_loop

@@start_parse:

    call parser_parse_expr
    test ax, ax
    jz @@loop_error_no_value
    mov [expr_ptr], ax

    call parser_parse_block
    test ax, ax
    jz @@loop_error_no_block
    mov [block_ptr], ax

    push INSTRUCTION_TYPE_LOOP
    call instruction_new
    mov es, ax

    mov ax, [expr_ptr]
    mov [es:INSTRUCTION_LOOP_OFF_EXPR], ax

    mov ax, [block_ptr]
    mov [es:INSTRUCTION_LOOP_OFF_BLOCK], ax

    mov ax, es
    jmp @@end_parse

@@loop_error_no_value:
    push [file_idx]
    push offset parser_error_invalid_token
    call parser_error
    jmp @@end_parse

@@loop_error_no_block:
    push [file_idx]
    push offset parser_error_syntax_error
    call parser_error
    jmp @@end_parse

@@not_parse_loop:
    mov ax, 0
    jmp @@end_parse

@@end_parse:

    pop es
    add sp, 4
    pop bp
    ret
endp parser_parse_loop

; FIXME: Check if it's okay to return 0 or is it possibly an allocated segment (not in code)
; Returns a parsed instruction segment into ax, or 0 if not found
instruction_ptr = bp - 2
proc parser_parse_instruction
    push bp
    mov bp, sp
    sub sp, 2

    call parser_parse_assignment
    test ax, ax
    jnz parsed_instruction
    call parser_parse_show
    test ax, ax
    jnz parsed_instruction
    call parser_parse_if
    test ax, ax
    jnz parsed_instruction
    call parser_parse_loop
    test ax, ax
    jnz parsed_instruction

    ; If wasn't able to parse an instruction, return a NULL
    mov [word ptr instruction_ptr], 0
    jmp end_parse_instruction

parsed_instruction:
    mov [word ptr instruction_ptr], ax ; Save the instruction in the stack
    push 1
    call parser_expect_newline

end_parse_instruction:
    mov ax, [instruction_ptr]

    add sp, 2
    pop bp
    ret
endp parser_parse_instruction

proc parser_parse_block
    push bp
    mov bp, sp
    push bx
    push es

    ; FIXME: Be able to extend the list
    push 2 * (7 + 1)
    call heap_alloc
    mov es, ax

    push TOKEN_TYPE_LEFT_BRACE
    call parser_match
    test ax, ax
    jz block_parse_failed

    push 0 ; Don't error if we don't match the newline
    call parser_expect_newline

    mov bx, 0 ; Offset
parse_block_instruction:
    push TOKEN_TYPE_RIGHT_BRACE
    call parser_match
    test ax, ax
    jnz finish_block

    call parser_parse_instruction
    test ax, ax
    jz block_parse_failed

    ; Set the parsed instruction
    mov [es:bx], ax

    add bx, 2
    jmp parse_block_instruction

finish_block:
    ; Set a NUL at the end
    mov [word ptr es:bx], 0
    mov ax, es
    jmp block_parse_end

block_parse_failed:
    mov ax, 0

block_parse_end:

    pop es
    pop bx
    pop bp
    ret
endp parser_parse_block

; Parse all of the file
backtrack = bp - 2
proc parser_parse
    push bp
    mov bp, sp
    sub sp, 2
    push ax
    push bx

    ; If there's a newline at the start, ignore it
    push 0
    call parser_expect_newline

try_parse_instruction:
    ; Store in bx where to write to in the data segment
    mov bx, [word ptr amount_instructions]
    shl bx, 1 ; Multiply by 2 to align as word-sized ptr
    add bx, offset parsed_instructions ; Make bx relative to the instructions array

    ; Break from loop if we couldn't parse
    call parser_parse_instruction ; Returns pointer into ax
    test ax, ax
    jz instruction_parsing_failed

    ; Add to array and reloop
    mov [bx], ax ; Put instruction pointer into the address in bx
    inc [word ptr amount_instructions]
    jmp try_parse_instruction

    ; End of parsing
instruction_parsing_failed:
    ; Check here if you have a remainder of non-parsable code
    mov ax, [file_idx]
    mov [backtrack], ax

    call lex
    test ax, ax
    jz no_code_remainer

    ; If we got here, we have some remainder code that cannot be parsed, so we should error

    ; Reload to the place before the token
    push [backtrack]
    call file_set_idx

    ; Error
    push [file_idx]
    push offset parser_error_syntax_error
    call parser_error

no_code_remainer:

    pop bx
    pop ax
    add sp, 2
    pop bp
    ret
endp parser_parse

proc parser_delete
    push ax
    push bx

    mov ax, 0
    mov bx, offset parsed_instructions

@@loop_instructions:
    cmp ax, [amount_instructions]
    je @@end_loop_instructions

    push [bx]
    call instruction_delete

    inc ax
    add bx, 2
    jmp @@loop_instructions

@@end_loop_instructions:

    pop bx
    pop ax
    ret
endp parser_delete

; Interpreter procedures

object_type = bp + 4
proc object_new
    push bp
    mov bp, sp
    push es

    ; Allocate
    push OBJECT_MAX_SIZE
    call heap_alloc
    mov es, ax
    ; Add type
    mov ax, [object_type]
    mov [es:OBJECT_OFF_TYPE], al
    mov [word ptr es:OBJECT_OFF_REFCOUNT], 1
    ; Prepare for return
    mov ax, es

    pop es
    pop bp
    ret 2
endp object_new

; Increment refcount
object_ptr = bp + 4
proc object_ref
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [object_ptr]
    mov es, ax

    inc [word ptr es:OBJECT_OFF_REFCOUNT]

    pop es
    pop ax
    pop bp
    ret 2
endp object_ref

; Decrement refcount and maybe remove the object
object_ptr = bp + 4
proc object_deref
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [object_ptr]
    mov es, ax

    ; Decrement the amount of references we have
    dec [word ptr es:OBJECT_OFF_REFCOUNT]

    cmp [word ptr es:OBJECT_OFF_REFCOUNT], 0
    jne @@not_delete

    push es
    call heap_free

@@not_delete:

    pop es
    pop ax
    pop bp
    ret 2
endp object_deref

object_ptr = bp + 4
proc object_number_to_bool
    push bp
    mov bp, sp
    push es

    mov ax, [object_ptr]
    mov es, ax

    mov ax, [es:OBJECT_NUMBER_OFF_NUMBER]
    test ax, ax

    test ax, ax
    jnz @@object_truthy

    mov ax, 0
    jmp @@object_check_end

@@object_truthy:
    mov ax, 1

@@object_check_end:

    pop es
    pop bp
    ret 2
endp object_number_to_bool

object_ptr = bp + 4
proc object_to_bool
    push bp
    mov bp, sp
    push es

    mov ax, [object_ptr]
    mov es, ax

    mov al, [es:OBJECT_OFF_TYPE]

    cmp al, OBJECT_TYPE_NUMBER
    je @@choice_number

    call panic

@@choice_number:
    push es
    call object_number_to_bool

@@choice_end:

    pop es
    pop bp
    ret 2
endp object_to_bool

expr_ptr = bp + 4
number = bp - 2
proc expr_number_eval
    push bp
    mov bp, sp
    sub sp, 2
    push es

    ; Store the expr segment
    mov ax, [expr_ptr]
    mov es, ax

    ; Store the number
    mov ax, [es:EXPR_NUMBER_OFF_NUMBER]
    mov [number], ax

    ; Create a value
    push OBJECT_TYPE_NUMBER
    call object_new
    mov es, ax

    ; Set the value's number
    mov ax, [number]
    mov [es:OBJECT_NUMBER_OFF_NUMBER], ax

    ; Set ax to new value so we can return it
    mov ax, es

    pop es
    add sp, 2
    pop bp
    ret 2
endp expr_number_eval

expr_ptr = bp + 4
proc expr_var_eval
    push bp
    mov bp, sp
    push es

    mov ax, [expr_ptr]
    mov es, ax

    mov ax, [es:EXPR_VAR_OFF_NAME]
    push ax
    call interpreter_get_variable

    test ax, ax
    jnz found_variable

    ; If we got here, we didn't find a variable
    push offset runtime_error_variable_not_found
    call interpreter_runtime_error

found_variable:

    pop es
    pop bp
    ret 2
endp expr_var_eval

expr_ptr = bp + 4
operator_func = bp + 6
lhs_value = bp - 2
rhs_value = bp - 4
lhs_number = bp - 6
rhs_number = bp - 8
proc expr_binary_eval
    push bp
    mov bp, sp
    sub sp, 8
    push es

    ; Store the expr segment
    mov ax, [expr_ptr]
    mov es, ax

    ; Eval lhs
    push [es:EXPR_BINARY_OFF_LHS]
    call expr_eval
    mov [lhs_value], ax
    ; Eval rhs
    push [es:EXPR_BINARY_OFF_RHS]
    call expr_eval
    mov [rhs_value], ax

    ; FIXME: Make sure we have a number in lhs and rhs
    mov ax, [lhs_value]
    mov es, ax
    mov ax, [es:OBJECT_NUMBER_OFF_NUMBER]
    mov [lhs_number], ax

    mov ax, [rhs_value]
    mov es, ax
    mov ax, [es:OBJECT_NUMBER_OFF_NUMBER]
    mov [rhs_number], ax

    ; Dereference the lhs and rhs of the expression
    push [lhs_value]
    call object_deref
    push [rhs_value]
    call object_deref

    ; Create a value
    push OBJECT_TYPE_NUMBER
    call object_new
    mov es, ax

    ; Set the value's number
    push [rhs_number]
    push [lhs_number]
    mov ax, [operator_func]
    call ax
    mov [es:OBJECT_NUMBER_OFF_NUMBER], ax

    ; Set ax to new value so we can return it
    mov ax, es

    pop es
    add sp, 8
    pop bp
    ret 4
endp expr_binary_eval

lhs = bp + 4
rhs = bp + 6
proc operator_add_func
    push bp
    mov bp, sp

    mov ax, [lhs]
    add ax, [rhs]

    pop bp
    ret 4
endp operator_add_func

lhs = bp + 4
rhs = bp + 6
proc operator_sub_func
    push bp
    mov bp, sp

    mov ax, [lhs]
    sub ax, [rhs]

    pop bp
    ret 4
endp operator_sub_func

lhs = bp + 4
rhs = bp + 6
proc operator_mul_func
    push bp
    mov bp, sp
    push bx
    push dx

    ; TODO: Error here if we get back something to dx (overflow the number)

    xor dx, dx
    mov ax, [lhs]
    mov bx, [rhs]
    imul bx

    pop dx
    pop bx
    pop bp
    ret 4
endp operator_mul_func

lhs = bp + 4
rhs = bp + 6
proc operator_div_func
    push bp
    mov bp, sp
    push bx
    push dx

    ; TODO: DIV by 0 error

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

    pop dx
    pop bx
    pop bp
    ret 4
endp operator_div_func

lhs = bp + 4
rhs = bp + 6
proc operator_mod_func
    push bp
    mov bp, sp
    push bx
    push dx

    ; TODO: DIV by 0 error

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

    pop dx
    pop bx
    pop bp
    ret 4
endp operator_mod_func

expr_ptr = bp + 4
proc expr_add_eval
    push bp
    mov bp, sp

    push offset operator_add_func
    push [expr_ptr]
    call expr_binary_eval

    pop bp
    ret 2
endp expr_add_eval

expr_ptr = bp + 4
proc expr_sub_eval
    push bp
    mov bp, sp

    push offset operator_sub_func
    push [expr_ptr]
    call expr_binary_eval

    pop bp
    ret 2
endp expr_sub_eval

expr_ptr = bp + 4
proc expr_mul_eval
    push bp
    mov bp, sp

    push offset operator_mul_func
    push [expr_ptr]
    call expr_binary_eval

    pop bp
    ret 2
endp expr_mul_eval

expr_ptr = bp + 4
proc expr_div_eval
    push bp
    mov bp, sp

    push offset operator_div_func
    push [expr_ptr]
    call expr_binary_eval

    pop bp
    ret 2
endp expr_div_eval

expr_ptr = bp + 4
proc expr_mod_eval
    push bp
    mov bp, sp

    push offset operator_mod_func
    push [expr_ptr]
    call expr_binary_eval

    pop bp
    ret 2
endp expr_mod_eval

expr_ptr = bp + 4
neg_number = bp - 2
proc expr_neg_eval
    push bp
    mov bp, sp
    sub sp, 2
    push es

    ; FIXME: Verify it's a number

    mov ax, [expr_ptr]
    mov es, ax

    push [es:EXPR_SINGLE_OFF_INNER]
    call expr_eval

    mov es, ax
    mov ax, [es:OBJECT_NUMBER_OFF_NUMBER]
    neg ax
    mov [neg_number], ax

    push es
    call object_deref

    push OBJECT_TYPE_NUMBER
    call object_new

    mov es, ax
    mov ax, [neg_number]
    mov [es:OBJECT_NUMBER_OFF_NUMBER], ax

    ; Return new number object
    mov ax, es

    pop es
    add sp, 2
    pop bp
    ret 2
endp expr_neg_eval

expr_ptr = bp + 4
proc expr_eval
    push bp
    mov bp, sp
    push es

    mov ax, [expr_ptr]
    mov es, ax

    mov al, [es:EXPR_OFF_TYPE]

    cmp al, EXPR_TYPE_NUMBER
    je choice_eval_number
    cmp al, EXPR_TYPE_VAR
    je choice_eval_var
    cmp al, EXPR_TYPE_ADD
    je choice_eval_add
    cmp al, EXPR_TYPE_SUB
    je choice_eval_sub
    cmp al, EXPR_TYPE_MUL
    je choice_eval_mul
    cmp al, EXPR_TYPE_DIV
    je choice_eval_div
    cmp al, EXPR_TYPE_MOD
    je choice_eval_mod
    cmp al, EXPR_TYPE_NEG
    je choice_eval_neg

    ; If we got here, our type code is invalid
    call panic

choice_eval_number:
    push [expr_ptr]
    call expr_number_eval
    jmp end_choice_eval

choice_eval_var:
    push [expr_ptr]
    call expr_var_eval
    jmp end_choice_eval

choice_eval_add:
    push [expr_ptr]
    call expr_add_eval
    jmp end_choice_eval

choice_eval_sub:
    push [expr_ptr]
    call expr_sub_eval
    jmp end_choice_eval

choice_eval_mul:
    push [expr_ptr]
    call expr_mul_eval
    jmp end_choice_eval

choice_eval_div:
    push [expr_ptr]
    call expr_div_eval
    jmp end_choice_eval

choice_eval_mod:
    push [expr_ptr]
    call expr_mod_eval
    jmp end_choice_eval

choice_eval_neg:
    push [expr_ptr]
    call expr_neg_eval

end_choice_eval:

    pop es
    pop bp
    ret 2
endp expr_eval

instruction_ptr = bp + 4
proc instruction_assign_execute
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    ; Eval expr and return value into ax
    push [es:INSTRUCTION_ASSIGN_OFF_EXPR] ; value
    call expr_eval

    push ax ; Push the value
    push [es:INSTRUCTION_ASSIGN_OFF_KEY] ; key
    call interpreter_set_variable

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_assign_execute

instruction_ptr = bp + 4
proc instruction_show_execute
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    ; Eval expr and return value into ax
    push [es:INSTRUCTION_SHOW_OFF_EXPR]
    call expr_eval
    mov es, ax

    ; FIXME: Verify this is a number
    mov ax, [es:OBJECT_NUMBER_OFF_NUMBER]

    push ax
    call print_word
    call print_newline

    ; Dereference the object
    push es
    call object_deref

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_show_execute

instrcution_ptr = bp + 4
evaluated_object = bp - 2
proc instruction_if_execute
    push bp
    mov bp, sp
    sub sp, 2
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    push [es:INSTRUCTION_IF_OFF_EXPR]
    call expr_eval

    mov [word ptr evaluated_object], ax

    push [evaluated_object]
    call object_to_bool ; Returns to ax

    push [evaluated_object]
    call object_deref

    ; Check the boolean result
    test ax, ax
    jz @@if_expr_failed

    ; If we got here, the expression was truthy so we need to execute the block

    push [es:INSTRUCTION_IF_OFF_BLOCK]
    call block_execute
    jmp @@end_execute

@@if_expr_failed:
    cmp [word ptr es:INSTRUCTION_IF_OFF_ELSE_BLOCK], 0
    je @@end_execute

    ; If we got here, we have an else block we need to execute
    push [es:INSTRUCTION_IF_OFF_ELSE_BLOCK]
    call block_execute

@@end_execute:

    pop es
    pop ax
    add sp, 2
    pop bp
    ret 2
endp instruction_if_execute

instruction_ptr = bp + 4
evaluated_object = bp - 2
proc instruction_loop_execute
    push bp
    mov bp, sp
    sub sp, 2
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

@@try_loop:
    push [es:INSTRUCTION_LOOP_OFF_EXPR]
    call expr_eval

    mov [word ptr evaluated_object], ax

    push [evaluated_object]
    call object_to_bool ; Returns into ax

    push [evaluated_object]
    call object_deref

    ; Check if condition value was truthy
    test ax, ax
    jz @@loop_condition_failed

    ; If we got here, the expression was truthy so we need to execute the block
    push [es:INSTRUCTION_LOOP_OFF_BLOCK]
    call block_execute

    jmp @@try_loop

@@loop_condition_failed:

    pop es
    pop ax
    add sp, 2
    pop bp
    ret 2
endp instruction_loop_execute

instruction_ptr = bp + 4
proc instruction_execute
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    mov al, [es:INSTRUCTION_OFF_TYPE]

    cmp al, INSTRUCTION_TYPE_ASSIGN
    je choice_instruction_assign
    cmp al, INSTRUCTION_TYPE_SHOW
    je choice_instruction_show
    cmp al, INSTRUCTION_TYPE_IF
    je choice_instruction_if
    cmp al, INSTRUCTION_TYPE_LOOP
    je choice_instruction_loop

    ; If we got here nothing matched, so we need to panic
    call panic

    jmp end_instruction_choice

choice_instruction_assign:
    push es
    call instruction_assign_execute
    jmp end_instruction_choice

choice_instruction_show:
    push es
    call instruction_show_execute
    jmp end_instruction_choice

choice_instruction_if:
    push es
    call instruction_if_execute
    jmp end_instruction_choice

choice_instruction_loop:
    push es
    call instruction_loop_execute

end_instruction_choice:

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_execute

block_ptr = bp + 4
proc block_delete
    push bp
    mov bp, sp
    push ax
    push bx
    push es

    mov ax, [block_ptr]
    mov es, ax

    ; Loop until reached NULL
    mov bx, 0
@@remove_instruction:
    mov ax, [es:bx]
    test ax, ax
    jz @@end_remove_block

    push ax
    call instruction_delete

    add bx, 2
    jmp @@remove_instruction

@@end_remove_block:
    push es
    call heap_free

    pop es
    pop bx
    pop ax
    pop bp
    ret 2
endp block_delete

block_ptr = bp + 4
proc block_execute
    push bp
    mov bp, sp
    push ax
    push bx
    push es

    mov ax, [block_ptr]
    mov es, ax

    mov bx, 0
@@loop_block:
    mov ax, [es:bx]
    test ax, ax
    jz @@end_loop_block

    push ax
    call instruction_execute

    add bx, 2
    jmp @@loop_block

@@end_loop_block:

    pop es
    pop bx
    pop ax
    pop bp
    ret 2
endp block_execute

proc interpreter_execute
    push ax
    push bx

    mov ax, 0
    lea bx, [parsed_instructions]
execute_instruction:
    cmp ax, [amount_instructions]
    jge end_execute_instruction

    push [bx]
    call instruction_execute

    inc ax
    add bx, 2
    jmp execute_instruction

end_execute_instruction:

    pop bx
    pop ax
    ret
endp interpreter_execute

key = bp + 4
value = bp + 6
proc interpreter_set_variable
    push bp
    mov bp, sp
    push ax
    push bx
    push cx

    mov cx, 0
    lea bx, [variables]
@@find_variable:
    cmp cx, [word ptr amount_variables]
    je @@add_new_variable ; Add a new variable

    push [key]
    push [bx + 0]
    call cstrs_eq
    test ax, ax
    jz @@try_again

    ; Deref the old value
    push [bx + 2]
    call object_deref

    ; Set the new value
    mov ax, [key]
    mov [bx + 0], ax
    mov ax, [value]
    mov [bx + 2], ax
    jmp @@variable_was_set

@@try_again:

    inc cx
    add bx, 4
    jmp @@find_variable

@@add_new_variable:
    inc [word ptr amount_variables]
    mov ax, [key]
    mov [bx + 0], ax
    mov ax, [value]
    mov [bx + 2], ax

@@variable_was_set:

    pop cx
    pop bx
    pop ax
    pop bp
    ret 4
endp interpreter_set_variable

name_ptr = bp + 4
proc interpreter_get_variable
    push bp
    mov bp, sp
    push bx

    mov ax, 0
    lea bx, [variables]
@@find_variable:
    cmp ax, [word ptr amount_variables]
    je @@not_found_variable

    push [bx + 0]
    push [name_ptr]
    call cstrs_eq

    test ax, ax
    jnz @@found_variable

    inc ax
    add bx, 4
    jmp @@find_variable

@@found_variable:
    mov ax, [bx + 2]
    ; Add reference
    push ax
    call object_ref
    jmp @@end_find_variable

@@not_found_variable:
    mov ax, 0

@@end_find_variable:

    pop bx
    pop bp
    ret 2
endp interpreter_get_variable

message_ptr = bp + 4
proc interpreter_runtime_error
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    ; ParserError:
    mov ah, 09h
    lea dx, [byte ptr runtime_error_start]
    int 21h
    ; Some error message
    mov ah, 09h
    mov dx, [message_ptr]
    int 21h
    call print_newline

    ; FIXME: We shouldn't exit here, but this is our only way now
    mov ax, 4C00h
    int 21h

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
endp interpreter_runtime_error

start:
    mov ax, @data
    mov ds, ax

    call open_file

    call parser_parse
    call interpreter_execute
    call parser_delete

    call close_file

    ; exit
    mov ax, 4C00h
    int 21h
END start
