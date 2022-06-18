DATASEG
    ; Error related stuff
    file_error_line_message db " [line ", 0
    file_error_column_message db ", column ", 0
    file_error_end db "]: ", 0
    file_error_code_line_start db "> ", 0

    ; Lexer error related stuff
    lexer_error_start db "LexerError", 0
    ; Parser error related stuff
    parser_error_start db "ParserError", 0
    ; Interpreter error related stuff
    runtime_error_no_state_start db "RuntimeError: ", 0
    runtime_error_start db "RuntimeError", 0

    ; Parsing-related error messages (lexer+parser)
    error_message_invalid_token db "Invalid token", 0
    error_message_syntax_error db "Syntax error", 0
    error_message_expected_newline db "Expected newline", 0
    error_message_unexpected_newline db "Unxpected newline", 0
    error_message_unexpected_token db "Unxpected token", 0
    error_message_number_too_big db "Number too big", 0
    error_message_too_many_instructions db "Too many instructions", 0
    ; Runtime-relater error messages (interpreter)
    error_message_variable_not_found db "Variable not found", 0
    error_message_not_enough_arguments db "Not enough arguments", 0
    error_message_could_not_open_file db "Could not open file", 0
    error_message_div_by_zero db "Division by 0", 0
    error_message_mul_overflow db "Overflowed beyond 16-bit when multiplying", 0
    error_message_invalid_operator_types db "Invalid operator types", 0
    error_message_invalid_operator_type db "Invalid operator type", 0
    error_message_expected_number db "Expected number", 0
    error_message_expected_vector db "Expected vector", 0
    error_message_invalid_argument_values db "Invalid argument values", 0
    error_message_allocation_failure db "Allocation failure", 0
    error_message_too_many_variables db "Too many variables defined", 0
    error_message_chr_number_invalid db "Chr number invalid", 0
    error_message_string_not_char db "String not a char-string (a string of length 1)", 0

CODESEG
; Setup the screen before erroring
proc error_setup
    push ax
    push bx
    push dx

    ; Set to text mode
    mov ah, 0
    mov al, 03h
    int 10h
    ; Return to color white
    mov [graphics_color], 0Fh
    ; Set position of cursor to beggining
    mov ah, 2
    mov bh, 0
    mov dh, 0
    mov dl, 0
    int 10h

    pop dx
    pop bx
    pop ax
    ret
endp error_setup

; Print the rest of the line from the index onwards
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
    mov bh, 0
    mov bl, [graphics_color]
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

; Shows an error with line and column information, including showing the line the error occurred in
; Used as "template" for other error functions
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

    push [error_start_ptr]
    call print_data_cstr

    push offset file_error_line_message
    call print_data_cstr

    push [index]
    call file_get_line_col_of_index
    mov [line], ax
    mov [column], bx

    push [line]
    call print_word

    push offset file_error_column_message
    call print_data_cstr

    push [column]
    call print_word

    push offset file_error_end
    call print_data_cstr

    ; Some error message
    push [message_ptr]
    call print_data_cstr
    call print_newline

    push offset file_error_code_line_start
    call print_data_cstr

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

; Writes a message in the format "LexerError [line l, column c]: Error message" and exits
message_ptr = bp + 4
index = bp + 6
proc lexer_error
    push bp
    mov bp, sp
    push ax

    call error_setup

    push [index]
    push [message_ptr]
    push offset lexer_error_start
    call show_file_error

    call prepare_for_exit
    call exit_fail

    pop ax
    pop bp
    ret 2
endp lexer_error

; Writes a message in the format "ParserError [line l, column c]: Error message" and exits
message_ptr = bp + 4
index = bp + 6
proc parser_error
    push bp
    mov bp, sp
    push ax

    call error_setup

    push [index]
    push [message_ptr]
    push offset parser_error_start
    call show_file_error

    call prepare_for_exit
    call exit_fail

    pop ax
    pop bp
    ret 4
endp parser_error


; Print a minimized RuntimeError
; This version shows no line/column information and does not show the line the error occured in
message_ptr = bp + 4
proc runtime_error_no_state
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    call error_setup

    ; ParserError:

    push offset runtime_error_no_state_start
    call print_data_cstr
    ; Some error message
    push [message_ptr]
    call print_data_cstr
    call print_newline

    call prepare_for_exit
    call exit_fail

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
endp runtime_error_no_state

; Writes a message in the format "RuntimeError [line l, column c]: Error message" and exits
message_ptr = bp + 4
index = bp + 6
proc runtime_error
    push bp
    mov bp, sp
    push ax

    call error_setup

    push [index]
    push [message_ptr]
    push offset runtime_error_start
    call show_file_error

    call prepare_for_exit
    call exit_fail

    pop ax
    pop bp
    ret 4
endp runtime_error

