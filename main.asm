IDEAL
MODEL small
STACK 300h
DATASEG
    ; Misc
    PSP_segment dw ?

    ; File related
    file dw ? ; Stores handle
    file_idx dw 0 ; Stores index
    file_read_buffer db 40h dup(?) ; Small read buffer

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

    ; Keyword definitions
    keyword_show db TOKEN_TYPE_SHOW, 4, "Show"
    keyword_if db TOKEN_TYPE_IF, 2, "if"
    keyword_else db TOKEN_TYPE_ELSE, 4, "else"
    keyword_loop db TOKEN_TYPE_LOOP, 4, "loop"
    keyword_xline db TOKEN_TYPE_XLINE, 5, "XLine"
    keyword_yline db TOKEN_TYPE_YLINE, 5, "YLine"
    keyword_rect db TOKEN_TYPE_RECT, 4, "Rect"
    keyword_filledrect db TOKEN_TYPE_FILLEDRECT, 10, "FilledRect"
    keyword_setcolor db TOKEN_TYPE_SETCOLOR, 8, "SetColor"
    keyword_diagonalline db TOKEN_TYPE_DIAGONALLINE, 12, "DiagonalLine"
    keyword_setwritepos db TOKEN_TYPE_SETWRITEPOS, 11, "SetWritePos"
    keyword_chr db TOKEN_TYPE_CHR, 3, "chr"
    keywords dw offset keyword_show, offset keyword_if, offset keyword_else
             dw offset keyword_loop, offset keyword_xline, offset keyword_yline
             dw offset keyword_rect, offset keyword_filledrect, offset keyword_setcolor
             dw offset keyword_diagonalline, offset keyword_setwritepos, offset keyword_chr
    AMOUNT_KEYWORDS = 12

    single_byte_tokens db TOKEN_TYPE_EQU, '='
                       db TOKEN_TYPE_PLUS, '+'
                       db TOKEN_TYPE_MINUS, '-'
                       db TOKEN_TYPE_STAR, '*'
                       db TOKEN_TYPE_SLASH, '/'
                       db TOKEN_TYPE_PERCENT, '%'
                       db TOKEN_TYPE_LEFT_BRACE, '{'
                       db TOKEN_TYPE_RIGHT_BRACE, '}'
                       db TOKEN_TYPE_LEFT_PAREN, '('
                       db TOKEN_TYPE_RIGHT_PAREN, ')'
                       db TOKEN_TYPE_LESS_THAN, '<'
                       db TOKEN_TYPE_GREATER_THAN, '>'
                       db TOKEN_TYPE_EXCLAMATION_MARK, '!'
                       db TOKEN_TYPE_COMMA, ','
    AMOUNT_SINGLE_BYTE_TOKENS = 14

    double_byte_tokens db TOKEN_TYPE_EQU_EQU, "=="
                       db TOKEN_TYPE_EXCLAMATION_MARK_EQU, "!="
                       db TOKEN_TYPE_LESS_EQU, "<="
                       db TOKEN_TYPE_GREATER_EQU, ">="
                       db TOKEN_TYPE_AMPERSAND_AMPERSAND, "&&"
                       db TOKEN_TYPE_PIPE_PIPE, "||"
    AMOUNT_DOUBLE_BYTE_TOKENS = 6


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

    ; Binary operator information (maps token types to expr types)
    parser_product_operators db TOKEN_TYPE_STAR, EXPR_TYPE_MUL
                             db TOKEN_TYPE_SLASH, EXPR_TYPE_DIV
                             db TOKEN_TYPE_PERCENT, EXPR_TYPE_MOD
    AMOUNT_PARSER_PRODUCT_OPERATORS = 3

    parser_sum_operators db TOKEN_TYPE_PLUS, EXPR_TYPE_ADD
                         db TOKEN_TYPE_MINUS, EXPR_TYPE_SUB
    AMOUNT_PARSER_SUM_OPERATORS = 2

    parser_cmp_operators db TOKEN_TYPE_LESS_THAN, EXPR_TYPE_CMP_SMALLER
                         db TOKEN_TYPE_GREATER_THAN, EXPR_TYPE_CMP_BIGGER
                         db TOKEN_TYPE_LESS_EQU, EXPR_TYPE_CMP_SMALLER_EQUALS
                         db TOKEN_TYPE_GREATER_EQU, EXPR_TYPE_CMP_BIGGER_EQUALS
                         db TOKEN_TYPE_EQU_EQU, EXPR_TYPE_CMP_EQUALS
                         db TOKEN_TYPE_EXCLAMATION_MARK_EQU, EXPR_TYPE_CMP_NOT_EQUAL
    AMOUNT_PARSER_CMP_OPERATORS = 5

    parser_or_operators db TOKEN_TYPE_PIPE_PIPE, EXPR_TYPE_OR
    AMOUNT_PARSER_OR_OPERATORS = 1

    parser_and_operators db TOKEN_TYPE_AMPERSAND_AMPERSAND, EXPR_TYPE_AND
    AMOUNT_PARSER_AND_OPERATORS = 1

    ; Instruction parsing functions and metadata about the instructions produced from them
    ; The word is parse function offset
    ; The byte is a boolean telling us if we should have a newline after the instruction
    instruction_defs dw offset parser_parse_assignment
                     db 1
                     dw offset parser_parse_if
                     db 0
                     dw offset parser_parse_loop
                     db 0
                     dw offset parser_parse_xline
                     db 1
                     dw offset parser_parse_yline
                     db 1
                     dw offset parser_parse_rect
                     db 1
                     dw offset parser_parse_filledrect
                     db 1
                     dw offset parser_parse_diagonalline
                     db 1
                     dw offset parser_parse_show
                     db 1
                     dw offset parser_parse_setcolor
                     db 1
                     dw offset parser_parse_setwritepos
                     db 1
                     dw offset parser_parse_block
                     db 0
    AMOUNT_INSTRUCTION_DEFS = 12

    ; Stores parsed instructions
    MAX_AMOUNT_INSTRUCTIONS = 80h
    parsed_instructions dw MAX_AMOUNT_INSTRUCTIONS dup(?)
    amount_instructions dw 0

    ; Max amount of instructions you can parse into a block
    MAX_AMOUNT_BLOCK_INSTRUCTIONS = 20h

    ; Object type offsets
    OBJECT_TYPE_OFF_FN_DELETE = 0
    OBJECT_TYPE_OFF_FN_ADD = 2
    OBJECT_TYPE_OFF_FN_SUB = 4
    OBJECT_TYPE_OFF_FN_MUL = 6
    OBJECT_TYPE_OFF_FN_DIV = 8
    OBJECT_TYPE_OFF_FN_MOD = 10
    OBJECT_TYPE_OFF_FN_EQ = 12
    OBJECT_TYPE_OFF_FN_SMALLER = 14
    OBJECT_TYPE_OFF_FN_BIGGER = 16
    OBJECT_TYPE_OFF_FN_SMALLER_EQ = 18
    OBJECT_TYPE_OFF_FN_BIGGER_EQ = 20
    OBJECT_TYPE_OFF_FN_NEG = 22
    OBJECT_TYPE_OFF_FN_CHR = 24
    OBJECT_TYPE_OFF_FN_TO_BOOL = 26
    OBJECT_TYPE_OFF_FN_SHOW = 28

    ; Object types
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

    ; Object offsets
    OBJECT_OFF_TYPE = 0
    OBJECT_OFF_REFCOUNT = 2
    ; Number offsets
    OBJECT_NUMBER_OFF_NUMBER = 4
    ; String offsets
    OBJECT_STRING_OFF_SOURCE = 4
    OBJECT_STRING_OFF_LENGTH = 6
    ; Vector offsets
    OBJECT_VECTOR_OFF_X = 4
    OBJECT_VECTOR_OFF_Y = 6

    OBJECT_MAX_SIZE = 8

    ; Interpreter variables
    MAX_AMOUNT_VARIABLES = 20h
    variables dw MAX_AMOUNT_VARIABLES * 2 dup(?) ; Stores cstr-object pairs
    amount_variables dw 0
    ; Interpreter currently used color
    graphics_color db 0Fh ; Start color is white

    GRAPHICS_SCREEN_WIDTH = 320
    GRAPHICS_SCREEN_HEIGHT = 200

    GRAPHICS_SCREEN_LINE_WIDTH = 40
    GRAPHICS_SCREEN_AMOUNT_LINES = 24

    GRAPHICS_AMOUNT_COLORS = 16 ; We have 16 possible colors

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

    ; Panic related stuff
    panic_message db "* PANIC *", 0
CODESEG

; Misc functions

; Exit with error code 1
proc exit_fail
    push ax

    mov ah, 4Ch
    mov al, 1 ; Return code 1
    int 21h

    pop ax
    ret
endp exit_fail

; Print a panic message and exit
; Used in places we don't want or wont ever reach
proc panic
    ; Setup color and screen mode
    call error_setup
    ; Print panic message
    push offset panic_message
    call print_data_cstr
    call exit_fail
    ; We should never reach this code
    ret
endp panic

; Get the absolute value of a word
number = bp + 4
proc number_abs
    push bp
    mov bp, sp

    mov ax, [number]
    cmp ax, 0
    jl @@number_negative

    jmp @@end_abs

@@number_negative:
    neg ax

@@end_abs:

    pop bp
    ret 2
endp number_abs

; Get -1 if the number is smaller than 0 otherwise 1
number = bp + 4
proc number_get_direction
    push bp
    mov bp, sp

    cmp [word ptr number], 0
    jl @@number_negative

    mov ax, 1
    jmp @@end_abs

@@number_negative:
    mov ax, -1

@@end_abs:

    pop bp
    ret 2
endp number_get_direction

; Returns whether the number is >=start and <end
number = bp + 4
range_start = bp + 6
range_end = bp + 8
proc number_validate
    push bp
    mov bp, sp

    mov ax, [number]
    cmp ax, [range_start]
    jl @@number_invalid

    cmp ax, [range_end]
    jge @@number_invalid

    ; If we got here the number is valid
    mov ax, 1
    jmp @@number_validation_end

@@number_invalid:
    mov ax, 0

@@number_validation_end:

    pop bp
    ret 6
endp number_validate

; Returns whether the given char is a character that can start a variable
character = bp + 4
proc is_char_var_start
    push bp
    mov bp, sp

    ; Load character (only al can possibly have a value different from 0)
    mov ax, [character]

    cmp al, 'a'
    jl @@not_lowercase
    cmp al, 'z'
    jg @@not_lowercase

    jmp @@character_is_var_start

@@not_lowercase:
    cmp al, 'A'
    jl @@not_uppercase
    cmp al, 'Z'
    jg @@not_uppercase

    jmp @@character_is_var_start

@@not_uppercase:
    cmp al, '_'
    je @@character_is_var_start

    mov ax, 0
    jmp character_var_start_check_end

@@character_is_var_start:
    mov ax, 1

character_var_start_check_end:

    pop bp
    ret 2
endp is_char_var_start

; Returns whether the argument is a character that can be in the middle of a variable name (any character not at the start)
character = bp + 4
proc is_char_var
    push bp
    mov bp, sp

    push [character]
    call is_char_var_start
    test ax, ax
    jnz @@character_valid

    ; Load character (only al can possibly have a value different from 0)
    mov ax, [character]

    cmp al, '0'
    jl @@not_digit
    cmp al, '9'
    jg @@not_digit

@@character_valid:
    mov ax, 1
    jmp @@check_end

@@not_digit:
    mov ax, 0

@@check_end:

    pop bp
    ret 2
endp is_char_var

; Print a newline (carriage return + line feed)
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

; Print a cstr using a segment and an offset
string_segment = bp + 4
string_offset = bp + 6
proc print_cstr
    push bp
    mov bp, sp
    push ax
    push bx
    push es

    mov ax, [string_segment]
    mov es, ax

    mov bx, [string_offset] ; Start index
@@loop_chars:
    ; Get char
    mov al, [es:bx]
    ; If got terminating NUL, exit loop
    cmp al, 0
    je @@end_loop_chars

    push bx ; Because bx is used for the loop
    mov ah, 0Eh
    mov bh, 0
    mov bl, [graphics_color]
    int 10h
    pop bx

    inc bx
    jmp @@loop_chars

@@end_loop_chars:

    pop es
    pop bx
    pop ax
    pop bp
    ret 4
endp print_cstr

; Print cstr from data segment
string_offset = bp + 4
proc print_data_cstr
    push bp
    mov bp, sp

    push [string_offset]
    push ds
    call print_cstr

    pop bp
    ret 2
endp print_data_cstr

; Print a word
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
    jge @@printed_number_is_positive

    ; Print the negative sign
    mov ah, 0Eh
    mov al, '-'
    mov bh, 0
    mov bl, [graphics_color]
    int 10h

    ; Flip because we want to print the positive after the '-'
    neg [word ptr word_number]

@@printed_number_is_positive:
    mov ax, [word_number]
    mov cx, 0
    mov bx, 10

@@get_word_length:
    inc cx

    xor dx, dx
    div bx

    cmp ax, 0
    jne @@get_word_length

@@print_digit:
    ; Divide 10^length times
    mov ax, [word_number]
    mov bx, 10
    ; Divide until you make the result mod 10 equal the digit
    push cx
    dec cx
@@divide_number:
    cmp cx, 0
    je @@end_divide_number

    xor dx, dx
    div bx

    dec cx
    jmp @@divide_number
@@end_divide_number:
    pop cx
    ; Make the remainder the digit
    xor dx, dx
    div bx
    mov ax, dx

    ; Make the digit an ascii digit
    add al, '0'
    ; Print the negative
    mov ah, 0Eh
    mov bh, 0
    mov bl, [graphics_color]
    int 10h

    loop @@print_digit

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
endp print_word

; Allocate `amount` bytes and return the segment of the allocated block
amount = bp + 4
proc heap_alloc
    push bp
    mov bp, sp
    push bx

    mov ax, [amount]
    mov bl, 10h
    div bl
    test ah, ah
    jz @@exact_fit
    inc al

@@exact_fit:
    cbw
    mov bx, ax
    mov ah, 48h
    int 21h
    jnc @@allocation_success

    ; If we got here, we had an allocation failure
    push offset error_message_allocation_failure
    call runtime_error_no_state

@@allocation_success:

    pop bx
    pop bp
    ret 2
endp heap_alloc

; Free a heap-allocated segment
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

; Compares two cstrs (the segments of the cstrs) and returns whether they're equal
cstr1 = bp + 4
cstr2 = bp + 6
index = bp - 2
proc cstrs_eq
    push bp
    mov bp, sp
    sub sp, 2
    push bx
    push cx
    push es

    mov [word ptr index], 0

@@loop_cstrs:
    mov bx, [index]
    ; Get char from first string
    mov ax, [cstr1]
    mov es, ax
    mov cl, [es:bx]
    ; Get char from second string
    mov ax, [cstr2]
    mov es, ax
    mov ch, [es:bx]

    cmp cl, ch
    jne @@cstrs_not_equal

    ; If the characters are NULs
    cmp cl, 0
    jne @@cstrs_reloop

    ; If we got here, we matched (characters were all the same and NUL is in the same index)
    mov ax, 1
    jmp @@end_loop_cstrs

@@cstrs_reloop:

    inc [word ptr index]
    jmp @@loop_cstrs

@@cstrs_not_equal:
    mov ax, 0

@@end_loop_cstrs:

    pop es
    pop cx
    pop bx
    add sp, 2
    pop bp
    ret 4
endp cstrs_eq

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

; Waits for the user to press the ESC key
proc wait_for_user_end_execution
    push ax

@@loop_key_wait:
    mov ah, 0
    int 16h
    cmp ah, 1 ; If pressed escape exit
    jne @@loop_key_wait

    pop ax
    ret
endp wait_for_user_end_execution

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

; File procedures

; Opens our file using command line arguments if found
; Possibly errors
proc open_file
    push ax
    push dx
    push es

    mov ax, [PSP_segment]
    mov es, ax
    ; Check if there is any argument
    cmp [byte ptr es:80h], 0
    jne @@has_filename

    ; If we got here, there are not enough arguments
    push offset error_message_not_enough_arguments
    call runtime_error_no_state

@@has_filename:

    mov bx, 82h ; because 81h is where it starts but it starts with a line-feed
@@find_filename_end:
    cmp [byte ptr es:bx], 0Dh
    je @@found_filename_end

    inc bx
    jmp @@find_filename_end

@@found_filename_end:
    mov [byte ptr es:bx], 0 ; NUL-terminate the argument

    push ds
    mov ax, es
    mov ds, ax
    mov ah, 3Dh
    mov al, 0 ; readonly
    mov dx, 82h
    int 21h
    pop ds

    jnc @@file_open_succeeded

    ; If we couldn't open the file
    push offset error_message_could_not_open_file
    call runtime_error_no_state

@@file_open_succeeded:

    mov [file], ax

    pop es
    pop dx
    pop ax
    ret
endp open_file

; Closes our file
proc close_file
    push ax

    ; Interrupt for closing files
    mov ah, 3Eh
    mov bx, [file]
    int 21h

    pop ax
    ret
endp close_file

; Sets the read index of the file
; Changes both `file_idx` and the actual index of the file handle
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

; Reads `amount_bytes` bytes into `file_read_buffer`
; If file doesn't have at least `amount_bytes` bytes left,
; we return false, otherwise we return true
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
    lea dx, [file_read_buffer]
    int 21h

    ; If had error
    jc @@had_error

    ; if read less than wanted
    cmp ax, [amount_bytes]
    jb @@had_error

    mov ax, [amount_bytes]
    add [file_idx], ax

    mov ax, 1
    jmp @@no_error

@@had_error:
    push [backtrack]
    call file_set_idx
    mov ax, 0

@@no_error:

    pop dx
    pop cx
    pop bx
    add sp, 2
    pop bp
    ret 2
endp read_bytes

; Tries to read a single newline from the file and returns whether it managed to do so
; Advances the file if it found the newline
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

; Returns the line and column into ax and bx (respectively) given the parameter file index
; Used for error functions
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
    je @@end_read_file
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

; Lexer procedures

; Try to lex a newline (or more, bunched together) and return whether we managed to do so
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
    ja @@had_newlines

    mov ax, 0
    jmp @@finish_newline_lex

@@had_newlines:
    mov ax, 1

@@finish_newline_lex:

    add sp, 2
    pop bp
    ret
endp lex_newline

; Try to lex a number and return whether we managed to do so
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
    jz @@not_number

    mov al, [byte ptr file_read_buffer]
    cmp al, '0'
    jl @@not_number
    cmp al, '9'
    jg @@not_number

    mov [byte ptr token_type], TOKEN_TYPE_NUMBER
    mov ax, [backtrack]
    mov [token_start_idx], ax
    mov [word ptr token_length], 0

@@still_number:
    inc [word ptr token_length]

    push 1
    call read_bytes
    test ax, ax
    jz @@end_number

    mov bl, [byte ptr file_read_buffer]
    cmp bl, '0'
    jl @@end_number_go_prev
    cmp bl, '9'
    jg @@end_number_go_prev
    jmp @@still_number

@@end_number_go_prev:
    ; If we got here there was a non-number character (but there WAS a character), so we must go back 1 step to "unadvance" the character
    mov bx, [word ptr file_idx] ; because we stepped over a non-digit
    dec bx
    push bx
    call file_set_idx

@@end_number:
    mov ax, 1
    jmp end_number_lex

@@not_number:
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

; Try to lex the string and return whether we were to do so
backtrack = bp - 2
proc lex_string
    push bp
    mov bp, sp
    sub sp, 2

    mov ax, [file_idx]
    mov [backtrack], ax

    push 1
    call read_bytes
    test ax, ax
    jz @@lex_failed
    cmp [byte ptr file_read_buffer], '"'
    jne @@lex_failed

    ; If we got here, we can start lexing the string
    mov [byte ptr token_type], TOKEN_TYPE_STRING
    mov [word ptr token_length], 0
    mov ax, [file_idx]
    mov [word ptr token_start_idx], ax

@@loop_string:
    call file_read_newline
    test ax, ax
    jnz @@error_newline_reached
    push 1
    call read_bytes
    test ax, ax
    jz @@error_newline_reached
    cmp [byte ptr file_read_buffer + 0], '"'
    je @@end_loop_string

    inc [word ptr token_length]
    jmp @@loop_string

@@end_loop_string:
    mov ax, 1
    jmp @@lex_end

@@error_newline_reached:
    push [backtrack]
    push offset error_message_unexpected_newline
    call parser_error

@@lex_failed:
    push [backtrack]
    call file_set_idx
    mov ax, 0

@@lex_end:

    add sp, 2
    pop bp
    ret
endp lex_string

; Try to lex a keyword using the given information and return whether we managed to do so
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
    jb @@keyword_char_cmp

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

; Try to lex any of the keywords and return whether we managed to do so
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

; Try to lex a variable name and return whether we were able to do so
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
    jz @@not_var

    mov al, [file_read_buffer]
    cbw
    push ax
    call is_char_var_start
    test ax, ax
    jz @@not_var

    mov [byte ptr token_type], TOKEN_TYPE_VAR
    mov ax, [backtrack]
    mov [token_start_idx], ax
    mov [word ptr token_length], 0

@@still_var:
    ; Store backtrack
    mov ax, [file_idx]
    mov [backtrack], ax

    inc [word ptr token_length]

    ; Try to read byte
    push 1
    call read_bytes
    test ax, ax
    jz @@end_var

    ; After we found a byte, check if it's a variable character
    mov al, [file_read_buffer]
    cbw
    push ax
    call is_char_var
    test ax, ax
    jnz @@still_var

@@end_var:
    push [backtrack]
    call file_set_idx

    mov ax, 1
    jmp @@end_var_lex

@@not_var:
    push [backtrack]
    call file_set_idx
    mov ax, 0
    jmp @@end_var_lex

@@end_var_lex:

    add sp, 2
    pop bp
    ret
endp lex_var

; Try to lex a char and return whether you did
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
    mov [byte ptr token_type], al
    mov ax, [backtrack]
    mov [token_start_idx], ax
    mov [word ptr token_length], 1
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

; Try to lex any of the two byte tokens (like == and !=) and return whether you did
backtrack = bp - 2
proc lex_double_byte_tokens
    push bp
    mov bp, sp
    sub sp, 2
    push bx
    push cx

    mov ax, [file_idx]
    mov [backtrack], ax

    push 2
    call read_bytes
    test ax, ax
    jz @@lex_failed

    mov ax, 0
    mov bx, offset double_byte_tokens
@@loop_tokens:
    cmp ax, AMOUNT_DOUBLE_BYTE_TOKENS
    je @@lex_failed
    mov cl, [bx + 1]
    cmp [byte ptr file_read_buffer + 0], cl
    jne @@not_matched_definition
    mov cl, [bx + 2]
    cmp [byte ptr file_read_buffer + 1], cl
    jne @@not_matched_definition

    ; If we got here, we matched the token
    jmp @@matched

@@not_matched_definition:

    inc ax
    add bx, 1 + 2
    jmp @@loop_tokens

@@matched:
    mov al, [bx + 0] ; Get token type
    mov [token_type], al
    mov [word ptr token_length], 2
    mov ax, [backtrack]
    mov [token_start_idx], ax
    mov ax, 1 ; Return true
    jmp @@end_lex

@@lex_failed:
    push [backtrack]
    call file_set_idx
    mov ax, 0

@@end_lex:

    pop cx
    pop bx
    add sp, 2
    pop bp
    ret
endp lex_double_byte_tokens

; Try to lex any of the single byte tokens (like + and -)
backtrack = bp - 2
proc lex_single_byte_tokens
    push bp
    mov bp, sp
    sub sp, 2
    push bx
    push cx

    mov ax, [file_idx]
    mov [backtrack], ax

    push 1
    call read_bytes
    test ax, ax
    jz @@lex_failed

    mov ax, 0
    mov bx, offset single_byte_tokens
@@loop_tokens:
    cmp ax, AMOUNT_SINGLE_BYTE_TOKENS
    je @@lex_failed
    mov cl, [bx + 1]
    cmp [byte ptr file_read_buffer + 0], cl
    je @@matched

    inc ax
    add bx, 1 + 1
    jmp @@loop_tokens

@@matched:
    mov al, [bx + 0] ; Get token type
    mov [token_type], al
    mov [word ptr token_length], 1
    mov ax, [backtrack]
    mov [token_start_idx], ax
    mov ax, 1 ; Return true
    jmp @@end_lex

@@lex_failed:
    push [backtrack]
    call file_set_idx
    mov ax, 0

@@end_lex:

    pop cx
    pop bx
    add sp, 2
    pop bp
    ret
endp lex_single_byte_tokens

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

    call wait_for_user_end_execution
    call exit_fail

    pop ax
    pop bp
    ret 2
endp lexer_error

; Returns whether we found a comment and advances it if we did
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

; Advances whitespace in the file (tabs and spaces)
backtrack = bp - 2
proc remove_whitespace
    push bp
    mov bp, sp
    sub sp, 2
    push ax

@@keep_removing:
    mov ax, [file_idx]
    mov [backtrack], ax
    call remove_comment
    test ax, ax
    jnz @@keep_removing
    push 1
    call read_bytes
    test ax, ax
    jz @@end_removing
    cmp [byte ptr file_read_buffer], ' '
    je @@keep_removing
    cmp [byte ptr file_read_buffer], 9 ; Tab
    je @@keep_removing
    push [backtrack]
    call file_set_idx
@@end_removing:

    pop ax
    add sp, 2
    pop bp
    ret
endp remove_whitespace

; Lexes a token, stores it in the global token and advances the file index
; Returns false if we encountered an EOF, otherwise returns true
; Errors if we can't lex anything
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
    jz @@lex_failed

    call lex_newline
    test ax, ax
    jnz @@end_lex

    call lex_number
    test ax, ax
    jnz @@end_lex

    call lex_string
    test ax, ax
    jnz @@end_lex

    call lex_keywords
    test ax, ax
    jnz @@end_lex

    call lex_var
    test ax, ax
    jnz @@end_lex

    call lex_double_byte_tokens
    test ax, ax
    jnz @@end_lex

    call lex_single_byte_tokens
    test ax, ax
    jnz @@end_lex

    push offset error_message_invalid_token
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

    call wait_for_user_end_execution
    call exit_fail

    pop ax
    pop bp
    ret 4
endp parser_error

; Returns whether we matched a token type and does nothing if we didn't
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
    jz @@not_matched

    ; Load expected token type into al and parsed type into ah
    mov ax, [expected_type]
    mov ah, [token_type]
    cmp al, ah
    jne @@not_matched

    ; If we got here we do match
    mov ax, 1
    jmp @@end_match

@@not_matched:
    push [backtrack]
    call file_set_idx
    mov ax, 0

@@end_match:

    add sp, 2
    pop bp
    ret 2
endp parser_match

; Checks for a newline or EOF and if found neither,
; depending on `should_error`, it either returns whether
; it found a newline, or errors when it didn't find them
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
    push offset error_message_expected_newline
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

; Parse the number expression
number = bp - 2
backtrack = bp - 4
proc parser_parse_expr_number
    push bp
    mov bp, sp
    sub sp, 4
    push es

    mov ax, [file_idx]
    mov [backtrack], ax

    ; Try to match a number token
    push TOKEN_TYPE_NUMBER
    call parser_match
    test ax, ax
    jz @@parse_finish

    call token_to_number ; Moves the actual number into ax
    mov [number], ax

    push [backtrack]
    push EXPR_TYPE_NUMBER
    call expr_new
    mov es, ax

    mov ax, [number]
    mov [es:EXPR_NUMBER_OFF_NUMBER], ax

    ; Move the address of the expr to ax so we can return it
    mov ax, es
    jmp @@parse_finish

@@parse_failed:
    mov ax, 0

@@parse_finish:

    pop es
    add sp, 4
    pop bp
    ret
endp parser_parse_expr_number

; Parse the "string" expression
string = bp - 2
backtrack = bp - 4
proc parser_parse_expr_string
    push bp
    mov bp, sp
    sub sp, 4
    push es

    mov ax, [file_idx]
    mov [backtrack], ax

    ; Try to match a string token
    push TOKEN_TYPE_STRING
    call parser_match
    test ax, ax
    jz @@not_string

    call token_to_cstr ; Moves the allocated string segment into ax
    mov [string], ax

    push [backtrack]
    push EXPR_TYPE_STRING
    call expr_new
    mov es, ax

    mov ax, [string]
    mov [es:EXPR_STRING_OFF_SOURCE], ax
    mov ax, [token_length]
    mov [es:EXPR_STRING_OFF_LENGTH], ax

    ; Move the address of the expr to ax so we can return it
    mov ax, es
    jmp @@parse_end

@@not_string:
    mov ax, 0

@@parse_end:

    pop es
    add sp, 4
    pop bp
    ret
endp parser_parse_expr_string

; Parse the (x, y) expression
lhs_ptr = bp - 2
rhs_ptr = bp - 4
backtrack = bp - 6
proc parser_parse_expr_vector
    push bp
    mov bp, sp
    sub sp, 6
    push es

    mov ax, [file_idx]
    mov [backtrack], ax

    push TOKEN_TYPE_LEFT_PAREN
    call parser_match
    test ax, ax
    jz @@parse_failed

    call parser_parse_expr
    test ax, ax
    jz @@parse_failed
    mov [lhs_ptr], ax

    push TOKEN_TYPE_COMMA
    call parser_match
    test ax, ax
    jz @@parse_failed_delete_lhs

    call parser_parse_expr
    test ax, ax
    jz @@error_expected_rhs
    mov [rhs_ptr], ax

    push TOKEN_TYPE_RIGHT_PAREN
    call parser_match
    test ax, ax
    jz @@error_syntax_error

    ; If we got here we parsed everything
    push [backtrack]
    push EXPR_TYPE_VECTOR
    call expr_new
    mov es, ax
    ; Set lhs and rhs
    mov ax, [lhs_ptr]
    mov [es:EXPR_VECTOR_OFF_X], ax
    mov ax, [rhs_ptr]
    mov [es:EXPR_VECTOR_OFF_Y], ax

    mov ax, es
    jmp @@parse_end

@@parse_failed_delete_lhs:
    push [lhs_ptr]
    call expr_delete

@@parse_failed:
    mov ax, 0
    push [backtrack]
    call file_set_idx
    jmp @@parse_end

@@error_syntax_error:
    push [rhs_ptr]
    call expr_delete
@@error_expected_rhs:
    push [lhs_ptr]
    call expr_delete

    push [file_idx]
    push offset error_message_syntax_error
    call parser_error

@@parse_end:

    pop es
    add sp, 6
    pop bp
    ret
endp parser_parse_expr_vector

; Parse the (expr) expression
inner_expr_ptr = bp - 2
proc parser_parse_expr_paren
    push bp
    mov bp, sp
    sub sp, 2
    push es

    push TOKEN_TYPE_LEFT_PAREN
    call parser_match
    test ax, ax
    jnz @@matched_paren

    mov ax, 0
    jmp @@end_parse_paren

@@matched_paren:
    call parser_parse_expr
    test ax, ax
    jz @@error_expected_expr

    ; If we got here we parsed an expression
    mov [inner_expr_ptr], ax

    push TOKEN_TYPE_RIGHT_PAREN
    call parser_match
    test ax, ax
    jz @@error_expected_right_paren

    mov ax, [inner_expr_ptr]
    jmp @@end_parse_paren

@@error_expected_expr:
@@error_expected_right_paren:
    push [file_idx]
    push offset error_message_syntax_error
    call parser_error

@@end_parse_paren:

    pop es
    add sp, 2
    pop bp
    ret
endp parser_parse_expr_paren

; General function for parsing prefix operators like `neg` or `not`
start_token_type = bp + 4
expr_type = bp + 6
inner_expr_ptr = bp - 2
backtrack = bp - 4
proc parser_parse_expr_prefix
    push bp
    mov bp, sp
    sub sp, 4
    push es

    mov ax, [file_idx]
    mov [backtrack], ax

    push [start_token_type]
    call parser_match
    test ax, ax
    jnz @@matched_token

    mov ax, 0
    jmp @@end_parse

@@matched_token:
    call parser_parse_expr_single
    test ax, ax
    jz @@error_expected_expr

    ; If we got here we parsed an expression
    mov [inner_expr_ptr], ax
    push [backtrack]
    push [expr_type]
    call expr_new
    mov es, ax
    mov ax, [inner_expr_ptr]
    mov [es:EXPR_SINGLE_OFF_INNER], ax
    mov ax, es
    jmp @@end_parse

@@error_expected_expr:
    push [file_idx]
    push offset error_message_syntax_error
    call parser_error

@@end_parse:

    pop es
    add sp, 4
    pop bp
    ret 4
endp parser_parse_expr_prefix

; Parse the `-value` operator
proc parser_parse_expr_neg
    push EXPR_TYPE_NEG
    push TOKEN_TYPE_MINUS
    call parser_parse_expr_prefix
    ret
endp parser_parse_expr_neg

; Parse the `!`
proc parser_parse_expr_not
    push EXPR_TYPE_NOT
    push TOKEN_TYPE_EXCLAMATION_MARK
    call parser_parse_expr_prefix
    ret
endp parser_parse_expr_not

; Parse the `chr`
proc parser_parse_expr_chr
    push EXPR_TYPE_CHR
    push TOKEN_TYPE_CHR
    call parser_parse_expr_prefix
    ret
endp parser_parse_expr_chr

; Parse a variable
var_name = bp - 2
backtrack = bp - 4
proc parser_parse_expr_var
    push bp
    mov bp, sp
    sub sp, 4
    push es

    mov ax, [file_idx]
    mov [backtrack], ax

    push TOKEN_TYPE_VAR
    call parser_match
    test ax, ax
    jz @@not_found

    call token_to_cstr
    mov [var_name], ax

    push [backtrack]
    push EXPR_TYPE_VAR
    call expr_new
    mov es, ax

    mov ax, [var_name]
    mov [es:EXPR_VAR_OFF_NAME], ax

    mov ax, es
    jmp @@end_parse

@@not_found:
    mov ax, 0

@@end_parse:

    pop es
    add sp, 4
    pop bp
    ret
endp parser_parse_expr_var

; Parse values/variables
proc parser_parse_expr_single
    call parser_parse_expr_number
    test ax, ax
    jnz @@end_parse
    call parser_parse_expr_string
    test ax, ax
    jnz @@end_parse
    call parser_parse_expr_vector
    test ax, ax
    jnz @@end_parse
    call parser_parse_expr_paren
    test ax, ax
    jnz @@end_parse
    call parser_parse_expr_neg
    test ax, ax
    jnz @@end_parse
    call parser_parse_expr_not
    test ax, ax
    jnz @@end_parse
    call parser_parse_expr_chr
    test ax, ax
    jnz @@end_parse
    call parser_parse_expr_var
    test ax, ax
    jnz @@end_parse

    ; If we got here, nothing was parsed
    mov ax, 0

@@end_parse:

    ret
endp parser_parse_expr_single

; General function to parse binary expressions
operator_info = bp + 4
amount_operators = bp + 6
child_function = bp + 8 ; Function that parses the lhs and rhs of this expr
left_ptr = bp - 2
right_ptr = bp - 4
new_expr_type = bp - 6
backtrack = bp - 8
proc parser_parse_expr_precedence
    push bp
    mov bp, sp
    sub sp, 8
    push bx
    push cx
    push es

    mov ax, [child_function]
    call ax
    mov [left_ptr], ax
    test ax, ax
    jnz @@parse_expr_loop

    jmp @@parse_expr_failed

@@parse_expr_loop:
    ; Store the index of the operator for the expr index
    mov ax, [file_idx]
    mov [backtrack], ax

    mov cx, [amount_operators]
    mov bx, [operator_info]
@@loop_operators:
    mov al, [bx + 0]
    cbw
    push ax
    call parser_match
    test ax, ax
    jnz @@matched ; If matched

    add bx, 2
    loop @@loop_operators

    ; If we got here we didn't match so we can finish parsing
    mov ax, [left_ptr]
    jmp @@parse_expr_finish

@@matched:
    mov al, [bx + 1] ; Get expr-type
    cbw
    mov [new_expr_type], ax

    ; Try to parse the expr after the operator
    mov ax, [child_function]
    call ax
    test ax, ax
    jz @@error_expected_expr
    mov [right_ptr], ax

    ; Create a new expr and put the information into it
    push [backtrack]
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
    push offset error_message_syntax_error
    call parser_error

@@parse_expr_failed:
    mov ax, 0

@@parse_expr_finish:

    pop es
    pop cx
    pop bx
    add sp, 8
    pop bp
    ret 6
endp parser_parse_expr_precedence

; Parses the `*` and `/` expressions
proc parser_parse_expr_product
    push offset parser_parse_expr_single
    push AMOUNT_PARSER_PRODUCT_OPERATORS
    push offset parser_product_operators
    call parser_parse_expr_precedence
    ret
endp parser_parse_expr_product

; Parses the `+` and `-` expressions
proc parser_parse_expr_sum
    push offset parser_parse_expr_product
    push AMOUNT_PARSER_SUM_OPERATORS
    push offset parser_sum_operators
    call parser_parse_expr_precedence
    ret
endp parser_parse_expr_sum

; Parses the `<`, `>`, `<=`, `>=`, `==` and `!=` expressions
proc parser_parse_expr_cmp
    push offset parser_parse_expr_sum
    push AMOUNT_PARSER_CMP_OPERATORS
    push offset parser_cmp_operators
    call parser_parse_expr_precedence
    ret
endp parser_parse_expr_cmp

; Parse the `&&` expression
proc parser_parse_expr_and
    push offset parser_parse_expr_cmp
    push AMOUNT_PARSER_AND_OPERATORS
    push offset parser_and_operators
    call parser_parse_expr_precedence
    ret
endp parser_parse_expr_and

; Parse the `||` expression
proc parser_parse_expr_or
    push offset parser_parse_expr_and
    push AMOUNT_PARSER_OR_OPERATORS
    push offset parser_or_operators
    call parser_parse_expr_precedence
    ret
endp parser_parse_expr_or

; The lowest-precedence expression parser
; Used for easier maintanance and readability of code
proc parser_parse_expr
    call parser_parse_expr_or
    ret
endp parser_parse_expr

; Parse the assignment instruction (var = expr)
expr_ptr = bp - 2
key_ptr = bp - 4
key_token = bp - 4 - TOKEN_SIZE
proc parser_parse_assignment
    push bp
    mov bp, sp
    sub sp, 4 + TOKEN_SIZE
    push es

    push TOKEN_TYPE_VAR
    call parser_match
    test ax, ax
    jnz @@var_found

    jmp @@parse_failed

@@var_found:

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
    mov [key_ptr], ax

    ; Create new instruction with type Assign
    push INSTRUCTION_TYPE_ASSIGN
    call instruction_new

    ; Make es point to the new instruction segment
    mov es, ax
    ; Store the key in the instruction
    mov ax, [key_ptr]
    mov [es:INSTRUCTION_ASSIGN_OFF_KEY], ax
    ; Store the number in the instruction
    mov ax, [expr_ptr]
    mov [es:INSTRUCTION_ASSIGN_OFF_EXPR], ax

    ; Move es back to ax so we can return the pointer to the instruction
    mov ax, es

    jmp @@end_parse

@@error_no_equal_sign:
@@error_no_value:
    push [file_idx]
    push offset error_message_invalid_token
    call parser_error
    jmp @@end_parse

@@parse_failed:
    mov ax, 0

@@end_parse:

    pop es
    add sp, 4 + TOKEN_SIZE
    pop bp
    ret
endp parser_parse_assignment

; Parse the `if` instruction
expr_ptr = bp - 2
instruction_ptr = bp - 4
else_instruction_ptr = bp - 6
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

    ; Try to parse the expression after the `if` token
    call parser_parse_expr
    test ax, ax
    jz @@if_error_no_value
    mov [expr_ptr], ax

    ; Advance a newline if there is one
    push 0
    call parser_expect_newline

    ; Try to parse the instruction following
    call parser_parse_instruction
    test ax, ax
    jz @@if_error_no_instruction
    mov [instruction_ptr], ax

    ; See if we have an else
    push TOKEN_TYPE_ELSE
    call parser_match
    test ax, ax
    jz @@not_found_else

    ; Advance a newline if there is one
    push 0
    call parser_expect_newline

    call parser_parse_instruction
    test ax, ax
    jz @@not_found_else_instruction
    mov [else_instruction_ptr], ax

    jmp @@create_if_instruction

@@not_found_else:
    mov [word ptr else_instruction_ptr], 0

@@create_if_instruction:

    push INSTRUCTION_TYPE_IF
    call instruction_new
    mov es, ax

    mov ax, [expr_ptr]
    mov [es:INSTRUCTION_IF_OFF_EXPR], ax

    mov ax, [instruction_ptr]
    mov [es:INSTRUCTION_IF_OFF_INSTRUCTION], ax

    mov ax, [else_instruction_ptr]
    mov [es:INSTRUCTION_IF_OFF_ELSE_INSTRUCTION], ax

    mov ax, es
    jmp @@end_parse

@@if_error_no_value:
    push [file_idx]
    push offset error_message_invalid_token
    call parser_error
    jmp @@end_parse

@@not_found_else_instruction:
@@if_error_no_instruction:
    push [file_idx]
    push offset error_message_syntax_error
    call parser_error
    jmp @@end_parse

@@not_parse_if:
    mov ax, 0

@@end_parse:

    pop es
    add sp, 6
    pop bp
    ret
endp parser_parse_if

; Parse the `loop` instruction
expr_ptr = bp - 2
instruction_ptr = bp - 4
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

    ; Advance a newline if there is one
    push 0
    call parser_expect_newline

    call parser_parse_instruction
    test ax, ax
    jz @@loop_error_no_instruction
    mov [instruction_ptr], ax

    push INSTRUCTION_TYPE_LOOP
    call instruction_new
    mov es, ax

    mov ax, [expr_ptr]
    mov [es:INSTRUCTION_LOOP_OFF_EXPR], ax

    mov ax, [instruction_ptr]
    mov [es:INSTRUCTION_LOOP_OFF_INSTRUCTION], ax

    mov ax, es
    jmp @@end_parse

@@loop_error_no_value:
    push [file_idx]
    push offset error_message_invalid_token
    call parser_error
    jmp @@end_parse

@@loop_error_no_instruction:
    push [file_idx]
    push offset error_message_syntax_error
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

; Parse an instruction with a one argument after the start token
start_token = bp + 4
instruction_type = bp + 6
argument = bp - 2
proc parser_parse_instruction_one_arg
    push bp
    mov bp, sp
    sub sp, 2
    push es

    push [start_token]
    call parser_match
    test ax, ax
    jz @@parse_failed

    call parser_parse_expr
    test ax, ax
    jz @@error_no_arg
    mov [arg1], ax

    ; If we got here, we parsed the instruction
    push [instruction_type]
    call instruction_new
    mov es, ax

    mov ax, [argument]
    mov [es:INSTRUCTION_ONE_ARG_OFF_ARG], ax

    mov ax, es
    jmp @@end_parse

@@error_no_arg:
    push [file_idx]
    push offset error_message_syntax_error
    call parser_error

@@parse_failed:
    mov ax, 0

@@end_parse:

    pop es
    add sp, 2
    pop bp
    ret 4
endp parser_parse_instruction_one_arg

; Parse an instruction with a two arguments after the start token
start_token = bp + 4
instruction_type = bp + 6
arg1 = bp - 2
arg2 = bp - 4
proc parser_parse_instruction_two_args
    push bp
    mov bp, sp
    sub sp, 4
    push es

    push [start_token]
    call parser_match
    test ax, ax
    jz @@parse_failed

    call parser_parse_expr
    test ax, ax
    jz @@error_no_arg1
    mov [arg1], ax

    push TOKEN_TYPE_COMMA
    call parser_match
    test ax, ax
    jz @@error_no_comma

    call parser_parse_expr
    test ax, ax
    jz @@error_no_arg2
    mov [arg2], ax

    ; If we got here, we parsed the instruction
    push [instruction_type]
    call instruction_new
    mov es, ax

    mov ax, [arg1]
    mov [es:INSTRUCTION_TWO_ARGS_OFF_ARG1], ax
    mov ax, [arg2]
    mov [es:INSTRUCTION_TWO_ARGS_OFF_ARG2], ax

    mov ax, es
    jmp @@end_parse

@@error_no_comma:
    push [file_idx]
    push offset error_message_unexpected_token
    call parser_error

@@error_no_arg1:
@@error_no_arg2:
    push [file_idx]
    push offset error_message_syntax_error
    call parser_error

@@parse_failed:
    mov ax, 0

@@end_parse:

    pop es
    add sp, 4
    pop bp
    ret 4
endp parser_parse_instruction_two_args

; Parses the `XLsine` instruction
proc parser_parse_xline
    push INSTRUCTION_TYPE_XLINE
    push TOKEN_TYPE_XLINE
    call parser_parse_instruction_two_args
    ret
endp parser_parse_xline

; Parses the `YLine` instruction
proc parser_parse_yline
    push INSTRUCTION_TYPE_YLINE
    push TOKEN_TYPE_YLINE
    call parser_parse_instruction_two_args
    ret
endp parser_parse_yline

; Parses the `Rect` instruction
proc parser_parse_rect
    push INSTRUCTION_TYPE_RECT
    push TOKEN_TYPE_RECT
    call parser_parse_instruction_two_args
    ret
endp parser_parse_rect

; Parses the `filledrect` instruction
proc parser_parse_filledrect
    push INSTRUCTION_TYPE_FILLEDRECT
    push TOKEN_TYPE_FILLEDRECT
    call parser_parse_instruction_two_args
    ret
endp parser_parse_filledrect

; Parses the `DiagonalLine` instruction
proc parser_parse_diagonalline
    push INSTRUCTION_TYPE_DIAGONALLINE
    push TOKEN_TYPE_DIAGONALLINE
    call parser_parse_instruction_two_args
    ret
endp parser_parse_diagonalline

; Parses the `show` instruction
proc parser_parse_show
    push INSTRUCTION_TYPE_SHOW
    push TOKEN_TYPE_SHOW
    call parser_parse_instruction_one_arg
    ret
endp parser_parse_show

; Parses the `SetColor` instruction
proc parser_parse_setcolor
    push INSTRUCTION_TYPE_SETCOLOR
    push TOKEN_TYPE_SETCOLOR
    call parser_parse_instruction_one_arg
    ret
endp parser_parse_setcolor

; Parses the `SetWritePos` instruction
proc parser_parse_setwritepos
    push INSTRUCTION_TYPE_SETWRITEPOS
    push TOKEN_TYPE_SETWRITEPOS
    call parser_parse_instruction_one_arg
    ret
endp parser_parse_setwritepos

; Try to parse a block of code: {instructions seperated by newline}
block_ptr = bp - 2
block_length = bp - 4
proc parser_parse_block
    push bp
    mov bp, sp
    sub sp, 4
    push bx
    push es

    ; Expect block to start with a '{'
    push TOKEN_TYPE_LEFT_BRACE
    call parser_match
    test ax, ax
    jnz @@found_left_brace

    ; If not found '{'
    jmp @@parse_failed

@@found_left_brace:
    push 0 ; Don't error if we don't match the newline
    call parser_expect_newline

    ; Allocate memory block to store instructions
    push 2 * MAX_AMOUNT_BLOCK_INSTRUCTIONS
    call heap_alloc
    mov es, ax

    mov [word ptr block_length], 0
    mov bx, 0 ; Offset
@@parse_block:
    push TOKEN_TYPE_RIGHT_BRACE
    call parser_match
    test ax, ax
    jnz @@finish_parse

    ; If we can't parse anymore (exceeded maximum amount of instructions)
    cmp [word ptr block_length], MAX_AMOUNT_BLOCK_INSTRUCTIONS
    je @@error_too_many_instructions

    call parser_parse_instruction
    test ax, ax
    jz @@parse_failed

    ; Set the parsed instruction
    mov [es:bx], ax

    add bx, 2
    inc [word ptr block_length]
    jmp @@parse_block

@@error_too_many_instructions:
    push [file_idx]
    push offset error_message_too_many_instructions
    call runtime_error

@@finish_parse:
    mov ax, es
    ; Store block
    mov [block_ptr], ax

    ; Create new block instruction
    push INSTRUCTION_TYPE_BLOCK
    call instruction_new
    mov es, ax
    ; Store length
    mov ax, [block_length]
    mov [es:INSTRUCTION_BLOCK_OFF_LENGTH], ax
    ; Store actual instructions
    mov ax, [block_ptr]
    mov [es:INSTRUCTION_BLOCK_OFF_INSTRUCTIONS], ax

    ; To return the instruction
    mov ax, es
    jmp @@parse_end

@@parse_failed:
    mov ax, 0

@@parse_end:

    pop es
    pop bx
    add sp, 4
    pop bp
    ret
endp parser_parse_block

; Returns a parsed instruction segment into ax, or 0 if no instruction found
instruction_ptr = bp - 2
backtrack = bp - 4
proc parser_parse_instruction
    push bp
    mov bp, sp
    sub sp, 4
    push bx
    push cx
    push es

    ; Store backtrack for the instruction's inner start position
    mov ax, [file_idx]
    mov [backtrack], ax

    lea bx, [instruction_defs]
    mov cx, AMOUNT_INSTRUCTION_DEFS
@@loop_instruction_defs:
    cmp cx, 0
    je @@end_loop_instruction_defs

    mov ax, [bx + 0]
    call ax
    test ax, ax
    jnz @@parsed_instruction

    dec cx
    add bx, 3
    jmp @@loop_instruction_defs

@@end_loop_instruction_defs:

    ; If wasn't able to parse an instruction, return a NULL
    mov [word ptr instruction_ptr], 0
    jmp @@end_parse

@@parsed_instruction:
    mov [word ptr instruction_ptr], ax ; Save the instruction in the stack

    ; Decide on parameters ofr `parser_expect_newline`
    cmp [byte ptr bx + 2], 0
    jne @@must_have_newline

    mov ax, 0
    jmp @@end_check_for_newline

@@must_have_newline:
    mov ax, 1

@@end_check_for_newline:
    push ax
    call parser_expect_newline

    ; Get instruction into es
    mov ax, [instruction_ptr]
    mov es, ax
    ; Put backtrack into instruction
    mov ax, [backtrack]
    mov [es:INSTRUCTION_OFF_FILE_INDEX], ax
    ; Return the instruction
    mov ax, es

@@end_parse:

    pop es
    pop cx
    pop bx
    add sp, 4
    pop bp
    ret
endp parser_parse_instruction

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

@@try_parse_instruction:
    cmp [word ptr amount_instructions], MAX_AMOUNT_INSTRUCTIONS
    je @@error_too_many_instructions

    ; Store in bx where to write to in the data segment
    mov bx, [word ptr amount_instructions]
    shl bx, 1 ; Multiply by 2 to align as word-sized ptr
    add bx, offset parsed_instructions ; Make bx relative to the instructions array

    ; Break from loop if we couldn't parse
    call parser_parse_instruction ; Returns pointer into ax
    test ax, ax
    jz @@parse_failed

    ; Add to array and reloop
    mov [bx], ax ; Put instruction pointer into the address in bx
    inc [word ptr amount_instructions]
    jmp @@try_parse_instruction

@@error_too_many_instructions:
    push [file_idx]
    push offset error_message_too_many_instructions
    call parser_error

    ; End of parsing
@@parse_failed:
    ; Check here if you have a remainder of non-parsable code
    mov ax, [file_idx]
    mov [backtrack], ax

    call lex
    test ax, ax
    jz @@no_code_remainer

    ; If we got here, we have some remainder code that cannot be parsed, so we should error

    ; Reload to the place before the token
    push [backtrack]
    call file_set_idx

    ; Error
    push [file_idx]
    push offset error_message_syntax_error
    call parser_error

@@no_code_remainer:

    pop bx
    pop ax
    add sp, 2
    pop bp
    ret
endp parser_parse

; Delete everything the parser stores
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

; Interpreter procedures

; Increment refcount of object
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
    mov [es:OBJECT_OFF_TYPE], ax
    mov [word ptr es:OBJECT_OFF_REFCOUNT], 1
    ; Prepare for return
    mov ax, es

    pop es
    pop bp
    ret 2
endp object_new

; Increment refcount of object
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
    push bx
    push es

    mov ax, [object_ptr]
    mov es, ax

    ; Decrement the amount of references we have
    dec [word ptr es:OBJECT_OFF_REFCOUNT]

    cmp [word ptr es:OBJECT_OFF_REFCOUNT], 0
    jne @@not_delete

    mov bx, [es:OBJECT_OFF_TYPE]
    mov ax, [bx + OBJECT_TYPE_OFF_FN_DELETE]
    test ax, ax
    jz @@end_delete ; If there is no delete function

    ; If we got here, we need to delete the object
    push [object_ptr]
    call ax

@@end_delete:

    push es
    call heap_free

@@not_delete:

    pop es
    pop bx
    pop ax
    pop bp
    ret 2
endp object_deref

; Returns whether an object is equal to another object
lhs_value = bp + 4
rhs_value = bp + 6
lhs_type = bp - 2
rhs_type = bp - 4
proc object_eq
    push bp
    mov bp, sp
    sub sp, 4
    push bx
    push es

    mov ax, [lhs_value]
    cmp ax, [rhs_value]
    jne @@not_same_ptr

    ; If they are the same object
    mov ax, 1
    jmp @@end_cmp

@@not_same_ptr:
    ; Get type of lhs
    mov ax, [lhs_value]
    mov es, ax
    mov ax, [es:OBJECT_OFF_TYPE]
    mov [lhs_type], ax
    ; Get type of rhs
    mov ax, [rhs_value]
    mov es, ax
    mov ax, [es:OBJECT_OFF_TYPE]
    mov [rhs_type], ax

    ; Find out if the types are the same
    mov bx, [lhs_type]
    cmp bx, [rhs_type]
    je @@type_match

    ; Types don't match
    mov ax, 0
    jmp @@end_cmp

@@type_match:
    mov ax, [bx + OBJECT_TYPE_OFF_FN_EQ]
    test ax, ax
    jnz @@had_eq_func

    ; If we don't have an EQ function the objects aren't equal
    mov ax, 0
    jmp @@end_cmp

@@had_eq_func:
    push [rhs_value]
    push [lhs_value]
    call ax ; Returns into ax

@@end_cmp:

    pop es
    pop bx
    add sp, 4
    pop bp
    ret 4
endp object_eq

; Returns whether the object truthy (e.g 9 is truthy but 0 isn't)
object_ptr = bp + 4
proc object_to_bool
    push bp
    mov bp, sp
    push bx
    push es

    mov ax, [object_ptr]
    mov es, ax

    mov bx, [es:OBJECT_OFF_TYPE]

    mov ax, [bx + OBJECT_TYPE_OFF_FN_TO_BOOL]
    test ax, ax
    jnz @@have_fn

    ; The default is for to_bool to return true
    mov ax, 1
    jmp @@end_to_bool

@@have_fn:
    push es
    call ax

@@end_to_bool:

    pop es
    pop bx
    pop bp
    ret 2
endp object_to_bool

; Expr evluation functions

; General function for evaluating type-dependent binary operators (like + or *)
expr_ptr = bp + 4
func_offset = bp + 6
lhs_value = bp - 2
rhs_value = bp - 4
lhs_type = bp - 6
rhs_type = bp - 8
proc expr_binary_operator_eval
    push bp
    mov bp, sp
    sub sp, 8
    push bx
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

    ; Get type of lhs
    mov ax, [lhs_value]
    mov es, ax
    mov ax, [es:OBJECT_OFF_TYPE]
    mov [lhs_type], ax
    ; Get type of rhs
    mov ax, [rhs_value]
    mov es, ax
    mov ax, [es:OBJECT_OFF_TYPE]
    mov [rhs_type], ax

    ; Find out if the types are the same
    mov bx, [lhs_type]
    cmp bx, [rhs_type]
    je @@type_match

    ; Reload expr_ptr into es because we overriden es
    mov ax, [expr_ptr]
    mov es, ax
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_invalid_operator_types
    call runtime_error

@@type_match:
    add bx, [func_offset] ; Make bx the index of the function pointer
    mov ax, [bx]

    ; Call the function
    push [expr_ptr]
    push [rhs_value]
    push [lhs_value]
    call ax

    ; Dereference the lhs and rhs of the expression
    push [lhs_value]
    call object_deref
    push [rhs_value]
    call object_deref

    pop es
    pop bx
    add sp, 8
    pop bp
    ret 4
endp expr_binary_operator_eval

; General function for evaluating type-dependent prefix operators
expr_ptr = bp + 4
func_offset = bp + 6
proc expr_prefix_operator_eval
    push bp
    mov bp, sp
    push bx
    push es

    mov ax, [expr_ptr]
    mov es, ax

    push [es:EXPR_SINGLE_OFF_INNER]
    call expr_eval
    mov es, ax

    mov bx, [es:OBJECT_OFF_TYPE]
    add bx, [func_offset]
    mov ax, [bx]

    ; Check if NEG is a function that's defined
    test ax, ax
    jnz @@had_fn

    mov ax, [expr_ptr]
    mov es, ax
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_invalid_operator_type
    call runtime_error

@@had_fn:

    push es
    call ax ; The returned value here (into ax) is the return value of the wrapper function

    push es
    call object_deref

    pop es
    pop bx
    pop bp
    ret 4
endp expr_prefix_operator_eval


; Evaluate a number expression and return the resulted number object
expr_ptr = bp + 4
proc expr_number_eval
    push bp
    mov bp, sp
    push es

    ; Store the expr segment
    mov ax, [expr_ptr]
    mov es, ax

    ; Create the number object
    push [es:EXPR_NUMBER_OFF_NUMBER]
    call object_number_new

    pop es
    pop bp
    ret 2
endp expr_number_eval


; Evaluate a string expression and return the resulted string: "string"
expr_ptr = bp + 4
source = bp - 2
string_length = bp - 4
proc expr_string_eval
    push bp
    mov bp, sp
    sub sp, 4
    push es

    ; Store the expr segment
    mov ax, [expr_ptr]
    mov es, ax

    ; Store the string
    mov ax, [es:EXPR_STRING_OFF_SOURCE]
    mov [source], ax
    mov ax, [es:EXPR_STRING_OFF_LENGTH]
    mov [string_length], ax

    ; Create a value
    push offset object_string_type
    call object_new
    mov es, ax

    ; Set the value's string
    mov ax, [source]
    mov [es:OBJECT_STRING_OFF_SOURCE], ax
    mov ax, [string_length]
    mov [es:OBJECT_STRING_OFF_LENGTH], ax

    ; Set ax to new value so we can return it
    mov ax, es

    pop es
    add sp, 4
    pop bp
    ret 2
endp expr_string_eval

; Evaluate a vector expression and return the resulted vector: (x, y)
expr_ptr = bp + 4
x_value = bp - 2
y_value = bp - 4
x_number = bp - 6
y_number = bp - 8
proc expr_vector_eval
    push bp
    mov bp, sp
    sub sp, 8
    push es

    ; Store the expr segment
    mov ax, [expr_ptr]
    mov es, ax

    push [es:EXPR_VECTOR_OFF_X]
    call expr_eval
    mov [x_value], ax

    push [es:EXPR_VECTOR_OFF_Y]
    call expr_eval
    mov [y_value], ax

    mov ax, [x_value]
    mov es, ax
    cmp [word ptr es:OBJECT_OFF_TYPE], offset object_number_type
    jne @@x_not_number
    push [x_value]
    call object_number_get
    mov [x_number], ax

    mov ax, [y_value]
    mov es, ax
    cmp [word ptr es:OBJECT_OFF_TYPE], offset object_number_type
    jne @@y_not_number
    push [y_value]
    call object_number_get
    mov [y_number], ax

    ; Create a vector object
    push offset object_vector_type
    call object_new
    mov es, ax
    ; Set the object's information
    mov ax, [x_number]
    mov [es:OBJECT_VECTOR_OFF_X], ax
    mov ax, [y_number]
    mov [es:OBJECT_VECTOR_OFF_Y], ax

    ; Set ax to new value so we can return it
    mov ax, es
    jmp @@end_eval

@@x_not_number:
    ; Set es to expr
    mov ax, [expr_ptr]
    mov es, ax
    ; Set es to x of vector expr
    mov ax, [es:EXPR_VECTOR_OFF_X]
    mov es, ax
    ; Call error with the index of x
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_expected_number
    call runtime_error

@@y_not_number:
    ; Set es to expr
    mov ax, [expr_ptr]
    mov es, ax
    ; Set es to y of vector expr
    mov ax, [es:EXPR_VECTOR_OFF_Y]
    mov es, ax
    ; Call error with the index of y
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_expected_number
    call runtime_error

@@end_eval:

    push [x_value]
    call object_deref
    push [y_value]
    call object_deref

    pop es
    add sp, 8
    pop bp
    ret 2
endp expr_vector_eval

; Evaluate a variable expression and return the matched value (if we can't find the variable, we error): var
expr_ptr = bp + 4
proc expr_var_eval
    push bp
    mov bp, sp
    push es

    mov ax, [expr_ptr]
    mov es, ax

    push [es:EXPR_VAR_OFF_NAME]
    call interpreter_get_variable

    test ax, ax
    jnz @@found_variable

    ; If we got here, we didn't find a variable
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_variable_not_found
    call runtime_error

@@found_variable:

    pop es
    pop bp
    ret 2
endp expr_var_eval

; Evaluate a `+` expression and return the resulted value: a + b
expr_ptr = bp + 4
proc expr_add_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_ADD
    push [expr_ptr]
    call expr_binary_operator_eval

    pop bp
    ret 2
endp expr_add_eval

; Evaluate a `-` expression and return the resulted value: a - b
expr_ptr = bp + 4
proc expr_sub_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_SUB
    push [expr_ptr]
    call expr_binary_operator_eval

    pop bp
    ret 2
endp expr_sub_eval

; Evaluate a `*` expression and return the resulted value: a * b
expr_ptr = bp + 4
proc expr_mul_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_MUL
    push [expr_ptr]
    call expr_binary_operator_eval

    pop bp
    ret 2
endp expr_mul_eval

; Evaluate a `/` expression and return the resulted value: a / b
expr_ptr = bp + 4
proc expr_div_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_DIV
    push [expr_ptr]
    call expr_binary_operator_eval

    pop bp
    ret 2
endp expr_div_eval

; Evaluate a `%` expression and return the resulted value: a % b
expr_ptr = bp + 4
proc expr_mod_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_MOD
    push [expr_ptr]
    call expr_binary_operator_eval

    pop bp
    ret 2
endp expr_mod_eval

; Evaluate a `==` expression and return the resulted value: a == b
expr_ptr = bp + 4
lhs_value = bp - 2
rhs_value = bp - 4
proc expr_cmp_equals_eval
    push bp
    mov bp, sp
    sub sp, 4
    push es

    mov ax, [expr_ptr]
    mov es, ax

    ; Eval lhs and store it
    push [es:EXPR_BINARY_OFF_LHS]
    call expr_eval
    mov [lhs_value], ax
    ; Eval rhs and store it
    push [es:EXPR_BINARY_OFF_RHS]
    call expr_eval
    mov [rhs_value], ax
    ; Check if the objects are equal
    push [rhs_value]
    push [lhs_value]
    call object_eq
    ; Create a number from the return value
    push ax
    call object_number_new ; The return value of this is the return value of this procedure

    ; Deref evaluated values
    push [lhs_value]
    call object_deref
    push [rhs_value]
    call object_deref

    pop es
    add sp, 4
    pop bp
    ret 2
endp expr_cmp_equals_eval

; Evaluate a `!=` expression and return the resulted value: a != b
expr_ptr = bp + 4
lhs_value = bp - 2
rhs_value = bp - 4
result = bp - 6
proc expr_cmp_not_equal_eval
    push bp
    mov bp, sp
    sub sp, 6
    push es

    mov ax, [expr_ptr]
    mov es, ax

    ; Eval lhs and store it
    push [es:EXPR_BINARY_OFF_LHS]
    call expr_eval
    mov [lhs_value], ax
    ; Eval rhs and store it
    push [es:EXPR_BINARY_OFF_RHS]
    call expr_eval
    mov [rhs_value], ax

    push [rhs_value]
    push [lhs_value]
    call object_eq
    mov [result], ax
    test ax, ax
    jz @@was_zero

    mov [word ptr result], 0
    jmp @@end_check

@@was_zero:
    mov [word ptr result], 1

@@end_check:

    ; The return value of this is the return value of this procedure
    push [result]
    call object_number_new

    ; Deref evaluated values
    push [lhs_value]
    call object_deref
    push [rhs_value]
    call object_deref

    pop es
    add sp, 6
    pop bp
    ret 2
endp expr_cmp_not_equal_eval

; Evaluate a `<` expression and return the resulted value: a < b
expr_ptr = bp + 4
proc expr_cmp_smaller_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_SMALLER
    push [expr_ptr]
    call expr_binary_operator_eval

    pop bp
    ret 2
endp expr_cmp_smaller_eval

; Evaluate a `>` expression and return the resulted value: a > b
expr_ptr = bp + 4
proc expr_cmp_bigger_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_BIGGER
    push [expr_ptr]
    call expr_binary_operator_eval

    pop bp
    ret 2
endp expr_cmp_bigger_eval

; Evaluate a `<=` expression and return the resulted value: a <= b
expr_ptr = bp + 4
proc expr_cmp_smaller_equals_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_SMALLER_EQ
    push [expr_ptr]
    call expr_binary_operator_eval

    pop bp
    ret 2
endp expr_cmp_smaller_equals_eval

; Evaluate a `>=` expression and return the resulted value: a >= b
expr_ptr = bp + 4
proc expr_cmp_bigger_equals_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_BIGGER_EQ
    push [expr_ptr]
    call expr_binary_operator_eval

    pop bp
    ret 2
endp expr_cmp_bigger_equals_eval

; Evaluate a `&&` expression and return the resulted value: a && b
expr_ptr = bp + 4
lhs_ptr = bp - 2
rhs_ptr = bp - 4
boolean_value = bp - 6
proc expr_and_eval
    push bp
    mov bp, sp
    sub sp, 6
    push es

    mov ax, [expr_ptr]
    mov es, ax

    mov [word ptr boolean_value], 0

    push [es:EXPR_BINARY_OFF_LHS]
    call expr_eval
    mov [lhs_ptr], ax

    push ax
    call object_to_bool
    test ax, ax
    jz @@left_failed

    push [es:EXPR_BINARY_OFF_RHS]
    call expr_eval
    mov [rhs_ptr], ax

    push ax
    call object_to_bool
    test ax, ax
    jz @@right_failed

    mov [word ptr boolean_value], 1

@@right_failed:
    push [rhs_ptr]
    call object_deref

@@left_failed:
    push [lhs_ptr]
    call object_deref

    ; Return the boolean value as a number object
    push [boolean_value]
    call object_number_new

    pop es
    add sp, 6
    pop bp
    ret 2
endp expr_and_eval

; Evaluate a `||` expression and return the resulted value: a || b
expr_ptr = bp + 4
lhs_ptr = bp - 2
rhs_ptr = bp - 4
boolean_value = bp - 6
proc expr_or_eval
    push bp
    mov bp, sp
    sub sp, 6
    push es

    mov ax, [expr_ptr]
    mov es, ax

    mov [word ptr boolean_value], 1

    push [es:EXPR_BINARY_OFF_LHS]
    call expr_eval
    mov [lhs_ptr], ax

    push ax
    call object_to_bool
    test ax, ax
    jnz @@left_success

    push [es:EXPR_BINARY_OFF_RHS]
    call expr_eval
    mov [rhs_ptr], ax

    push ax
    call object_to_bool
    test ax, ax
    jnz @@right_success

    push [lhs_ptr]
    call object_deref
    push [rhs_ptr]
    call object_deref

    mov [word ptr boolean_value], 0

@@right_success:
    push [rhs_ptr]
    call object_deref

@@left_success:
    push [lhs_ptr]
    call object_deref

    ; Return the boolean as an object
    push [boolean_value]
    call object_number_new

    pop es
    add sp, 6
    pop bp
    ret 2
endp expr_or_eval

; Evaluate a `!` expression and return the resulted value: !a
expr_ptr = bp + 4
evaluated_object = bp - 2
boolean_value = bp - 4
proc expr_not_eval
    push bp
    mov bp, sp
    sub sp, 4
    push es

    mov ax, [expr_ptr]
    mov es, ax

    mov [word ptr boolean_value], 1

    push [es:EXPR_SINGLE_OFF_INNER]
    call expr_eval
    mov [evaluated_object], ax

    push ax
    call object_to_bool
    test ax, ax
    jz @@object_not_truthy

    mov [word ptr boolean_value], 0

@@object_not_truthy:
    ; Remove inner object
    push [evaluated_object]
    call object_deref

    ; Return the boolean value as a number object
    push [boolean_value]
    call object_number_new

    pop es
    add sp, 4
    pop bp
    ret 2
endp expr_not_eval

; Evaluate a `-value` expression and return the resulted value: -a
expr_ptr = bp + 4
proc expr_neg_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_NEG
    push [expr_ptr]
    call expr_prefix_operator_eval

    pop bp
    ret 2
endp expr_neg_eval

; Evaluate a `chr` expression and return the resulted value: chr a
expr_ptr = bp + 4
proc expr_chr_eval
    push bp
    mov bp, sp

    push OBJECT_TYPE_OFF_FN_CHR
    push [expr_ptr]
    call expr_prefix_operator_eval

    pop bp
    ret 2
endp expr_chr_eval

; Evaluate an expression from a segment parameter and return the result object or error
expr_ptr = bp + 4
proc expr_eval
    push bp
    mov bp, sp
    push es

    mov ax, [expr_ptr]
    mov es, ax

    mov al, [es:EXPR_OFF_TYPE]

    ; Decide on the eval function using this switch case
    cmp al, EXPR_TYPE_NUMBER
    je @@choice_number
    cmp al, EXPR_TYPE_VAR
    je @@choice_var
    cmp al, EXPR_TYPE_ADD
    je @@choice_add
    cmp al, EXPR_TYPE_SUB
    je @@choice_sub
    cmp al, EXPR_TYPE_MUL
    je @@choice_mul
    cmp al, EXPR_TYPE_DIV
    je @@choice_div
    cmp al, EXPR_TYPE_MOD
    je @@choice_mod
    cmp al, EXPR_TYPE_NEG
    je @@choice_neg
    cmp al, EXPR_TYPE_CMP_EQUALS
    je @@choice_cmp_equals
    cmp al, EXPR_TYPE_CMP_NOT_EQUAL
    je @@choice_cmp_not_equal
    cmp al, EXPR_TYPE_CMP_SMALLER
    je @@choice_cmp_smaller
    cmp al, EXPR_TYPE_CMP_BIGGER
    je @@choice_cmp_bigger
    cmp al, EXPR_TYPE_CMP_SMALLER_EQUALS
    je @@choice_cmp_smaller_equals
    cmp al, EXPR_TYPE_CMP_BIGGER_EQUALS
    je @@choice_cmp_bigger_equals
    cmp al, EXPR_TYPE_AND
    je @@choice_and
    cmp al, EXPR_TYPE_OR
    je @@choice_or
    cmp al, EXPR_TYPE_NOT
    je @@choice_not
    cmp al, EXPR_TYPE_CHR
    je @@choice_chr
    cmp al, EXPR_TYPE_STRING
    je @@choice_string
    cmp al, EXPR_TYPE_VECTOR
    je @@choice_vector

    ; If we got here, our type is invalid
    call panic

@@choice_number:
    mov ax, offset expr_number_eval
    jmp @@end_choice

@@choice_var:
    mov ax, offset expr_var_eval
    jmp @@end_choice

@@choice_add:
    mov ax, offset expr_add_eval
    jmp @@end_choice

@@choice_sub:
    mov ax, offset expr_sub_eval
    jmp @@end_choice

@@choice_mul:
    mov ax, offset expr_mul_eval
    jmp @@end_choice

@@choice_div:
    mov ax, offset expr_div_eval
    jmp @@end_choice

@@choice_mod:
    mov ax, offset expr_mod_eval
    jmp @@end_choice

@@choice_neg:
    mov ax, offset expr_neg_eval
    jmp @@end_choice

@@choice_chr:
    mov ax, offset expr_chr_eval
    jmp @@end_choice

@@choice_cmp_equals:
    mov ax, offset expr_cmp_equals_eval
    jmp @@end_choice

@@choice_cmp_not_equal:
    mov ax, offset expr_cmp_not_equal_eval
    jmp @@end_choice

@@choice_cmp_smaller:
    mov ax, offset expr_cmp_smaller_eval
    jmp @@end_choice

@@choice_cmp_bigger:
    mov ax, offset expr_cmp_bigger_eval
    jmp @@end_choice

@@choice_cmp_smaller_equals:
    mov ax, offset expr_cmp_smaller_equals_eval
    jmp @@end_choice

@@choice_cmp_bigger_equals:
    mov ax, offset expr_cmp_bigger_equals_eval
    jmp @@end_choice

@@choice_and:
    mov ax, offset expr_and_eval
    jmp @@end_choice

@@choice_or:
    mov ax, offset expr_or_eval
    jmp @@end_choice

@@choice_not:
    mov ax, offset expr_not_eval
    jmp @@end_choice

@@choice_string:
    mov ax, offset expr_string_eval
    jmp @@end_choice

@@choice_vector:
    mov ax, offset expr_vector_eval

@@end_choice:

    push [expr_ptr]
    call ax

    pop es
    pop bp
    ret 2
endp expr_eval

; Instruction execute functions

; Execute the `=` code instruction (var = expr)
instruction_ptr = bp + 4
proc instruction_assign_execute
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    ; Eval expr and return value into ax
    push [es:INSTRUCTION_ASSIGN_OFF_EXPR] ; Push the expr segment
    call expr_eval ; Evaluate the expr

    push ax ; Push the value
    push [es:INSTRUCTION_ASSIGN_OFF_KEY] ; Push the key segment
    call interpreter_set_variable

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_assign_execute

; Execute the `if` code instruction
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

    push [es:INSTRUCTION_IF_OFF_INSTRUCTION]
    call instruction_execute
    jmp @@end_execute

@@if_expr_failed:
    cmp [word ptr es:INSTRUCTION_IF_OFF_ELSE_INSTRUCTION], 0
    je @@end_execute

    ; If we got here, we have an else block we need to execute
    push [es:INSTRUCTION_IF_OFF_ELSE_INSTRUCTION]
    call instruction_execute

@@end_execute:

    pop es
    pop ax
    add sp, 2
    pop bp
    ret 2
endp instruction_if_execute

; Execute the `loop` code instruction
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

    mov [evaluated_object], ax

    push [evaluated_object]
    call object_to_bool ; Returns into ax

    push [evaluated_object]
    call object_deref

    ; Check if condition value was truthy
    test ax, ax
    jz @@loop_condition_failed

    ; If we got here, the expression was truthy so we need to execute the block
    push [es:INSTRUCTION_LOOP_OFF_INSTRUCTION]
    call instruction_execute

    jmp @@try_loop

@@loop_condition_failed:

    pop es
    pop ax
    add sp, 2
    pop bp
    ret 2
endp instruction_loop_execute

; Execute an instruction that takes a position vector and size (number) as parameters
; Syntax: instruction (x, y), size
; This function takes information that helps it run in a general way but provide
; specific functionallity
instruction_ptr = bp + 4
exec_func = bp + 6
validate_func = bp + 8
arg1_value = bp - 2
arg2_value = bp - 4
start_x = bp - 6
start_y = bp - 8
line_length = bp - 10
proc instruction_line_execute
    push bp
    mov bp, sp
    sub sp, 6
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    ; Eval first argument
    push [es:INSTRUCTION_TWO_ARGS_OFF_ARG1]
    call expr_eval
    mov [arg1_value], ax
    ; Eval second argument
    push [es:INSTRUCTION_TWO_ARGS_OFF_ARG2]
    call expr_eval
    mov [arg2_value], ax

    ; Verify the first argument is a vector
    mov ax, [arg1_value]
    mov es, ax
    cmp [word ptr es:OBJECT_OFF_TYPE], offset object_vector_type
    jne @@error_arg1_not_vector
    ; Verify the second argument is a number
    mov ax, [arg2_value]
    mov es, ax
    cmp [word ptr es:OBJECT_OFF_TYPE], offset object_number_type
    jne @@error_arg2_not_number

    push [arg1_value]
    call object_vector_get_x
    mov [start_x], ax
    push [arg1_value]
    call object_vector_get_y
    mov [start_y], ax

    push [arg2_value]
    call object_number_get
    mov [line_length], ax

    push [arg1_value]
    call object_deref
    push [arg2_value]
    call object_deref

    mov ax, [validate_func]
    push [line_length]
    push [start_y]
    push [start_x]
    call ax
    test ax, ax
    jz @@error_invalid_arguments

    mov ax, [exec_func]
    push [line_length]
    push [start_y]
    push [start_x]
    call ax

    jmp @@end_execute

@@error_arg1_not_vector:
    ; Set es to arg1 expr
    mov ax, [instruction_ptr]
    mov es, ax
    mov ax, [es:INSTRUCTION_TWO_ARGS_OFF_ARG1]
    mov es, ax
    ; Error
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_expected_vector
    call runtime_error

@@error_arg2_not_number:
    ; Set es to arg2 expr
    mov ax, [instruction_ptr]
    mov es, ax
    mov ax, [es:INSTRUCTION_TWO_ARGS_OFF_ARG1]
    mov es, ax
    ; Error
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_expected_number
    call runtime_error

@@error_invalid_arguments:
    mov ax, [instruction_ptr]
    mov es, ax
    ; Error
    push [es:INSTRUCTION_OFF_FILE_INDEX]
    push offset error_message_invalid_argument_values
    call runtime_error

@@end_execute:

    pop es
    pop ax
    add sp, 6
    pop bp
    ret 6
endp instruction_line_execute

; Exectue the `XLine` code instruction
instruction_ptr = bp + 4
proc instruction_xline_execute
    push bp
    mov bp, sp

    push offset graphics_validate_xline
    push offset graphics_show_xline
    push [instruction_ptr]
    call instruction_line_execute

    pop bp
    ret 2
endp instruction_xline_execute

; Exectue the `YLine` code instruction
instruction_ptr = bp + 4
proc instruction_yline_execute
    push bp
    mov bp, sp

    push offset graphics_validate_yline
    push offset graphics_show_yline
    push [instruction_ptr]
    call instruction_line_execute

    pop bp
    ret 2
endp instruction_yline_execute

; Execute an instruction that takes position and size vectors as parameters
; Syntax: instruction (x, y), (w, h)
; This function takes information that helps it run in a general way but provide
; specific functionallity
instruction_ptr = bp + 4
exec_func = bp + 6
arg1_value = bp - 2
arg2_value = bp - 4
start_x = bp - 6
start_y = bp - 8
size_width = bp - 10
size_height = bp - 12
proc instruction_position_size_execute
    push bp
    mov bp, sp
    sub sp, 8
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    ; Eval first argument
    push [es:INSTRUCTION_TWO_ARGS_OFF_ARG1]
    call expr_eval
    mov [arg1_value], ax
    ; Eval second argument
    push [es:INSTRUCTION_TWO_ARGS_OFF_ARG2]
    call expr_eval
    mov [arg2_value], ax

    ; Verify the first argument is a vector
    mov ax, [arg1_value]
    mov es, ax
    cmp [word ptr es:OBJECT_OFF_TYPE], offset object_vector_type
    jne @@error_arg1_not_vector
    ; Verify the second argument is a number
    mov ax, [arg2_value]
    mov es, ax
    cmp [word ptr es:OBJECT_OFF_TYPE], offset object_vector_type
    jne @@error_arg2_not_vector

    ; Get the start x and y
    push [arg1_value]
    call object_vector_get_x
    mov [start_x], ax
    push [arg1_value]
    call object_vector_get_y
    mov [start_y], ax
    ; Get the end x and y
    push [arg2_value]
    call object_vector_get_x
    mov [size_width], ax
    push [arg2_value]
    call object_vector_get_y
    mov [size_height], ax

    ; Deref values used
    push [arg1_value]
    call object_deref
    push [arg2_value]
    call object_deref

    push [size_height]
    push [size_width]
    push [start_y]
    push [start_x]
    call graphics_validate_start_size_vectors
    test ax, ax
    jz @@error_invalid_arguments

    push [size_height]
    push [size_width]
    push [start_y]
    push [start_x]
    mov ax, [exec_func]
    call ax

    jmp @@end_execute

@@error_arg1_not_vector:
    ; Set es to arg1 expr
    mov ax, [instruction_ptr]
    mov es, ax
    mov ax, [es:INSTRUCTION_TWO_ARGS_OFF_ARG1]
    mov es, ax
    ; Error
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_expected_vector
    call runtime_error

@@error_arg2_not_vector:
    ; Set es to arg2 expr
    mov ax, [instruction_ptr]
    mov es, ax
    mov ax, [es:INSTRUCTION_TWO_ARGS_OFF_ARG2]
    mov es, ax
    ; Error
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_expected_vector
    call runtime_error

@@error_invalid_arguments:
    mov ax, [instruction_ptr]
    mov es, ax
    ; Error
    push [es:INSTRUCTION_OFF_FILE_INDEX]
    push offset error_message_invalid_argument_values
    call runtime_error

@@end_execute:

    pop es
    pop ax
    add sp, 8
    pop bp
    ret 4
endp instruction_position_size_execute

; Exectue the `Rect` code instruction
instruction_ptr = bp + 4
proc instruction_rect_execute
    push bp
    mov bp, sp

    push offset graphics_show_rect
    push [instruction_ptr]
    call instruction_position_size_execute

    pop bp
    ret 2
endp instruction_rect_execute

; Exectue the `FilledRect` code instruction
instruction_ptr = bp + 4
proc instruction_filledrect_execute
    push bp
    mov bp, sp

    push offset graphics_show_filledrect
    push [instruction_ptr]
    call instruction_position_size_execute

    pop bp
    ret 2
endp instruction_filledrect_execute

; Exectue the `DiagonalLine` code instruction
instruction_ptr = bp + 4
proc instruction_diagonalline_execute
    push bp
    mov bp, sp

    push offset graphics_show_diagonalline
    push [instruction_ptr]
    call instruction_position_size_execute

    pop bp
    ret 2
endp instruction_diagonalline_execute

; Exectue the `Show` code instruction
instruction_ptr = bp + 4
proc instruction_show_execute
    push bp
    mov bp, sp
    push ax
    push bx
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    ; Eval expr and return value into ax
    push [es:INSTRUCTION_ONE_ARG_OFF_ARG]
    call expr_eval
    mov es, ax

    mov bx, [es:OBJECT_OFF_TYPE]

    mov ax, [bx + OBJECT_TYPE_OFF_FN_SHOW]
    test ax, ax
    jnz @@end_show

    ; If we don't have a SHOW function defined
    call panic

@@end_show:

    ; Call the Show function
    push es
    call ax

    ; Show newline after the print
    call print_newline

    ; Dereference the object
    push es
    call object_deref

    pop es
    pop bx
    pop ax
    pop bp
    ret 2
endp instruction_show_execute

; Exectue the `SetColor` code instruction
instruction_ptr = bp + 4
arg_ptr = bp - 2
color = bp - 3
proc instruction_setcolor_execute
    push bp
    mov bp, sp
    sub sp, 3
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    push [es:INSTRUCTION_ONE_ARG_OFF_ARG]
    call expr_eval
    mov [arg_ptr], ax
    mov es, ax ; Store the argument object segment
    ; Verify the object is a number
    cmp [word ptr es:OBJECT_OFF_TYPE], offset object_number_type
    jne @@error_arg_not_number
    ; Get the actual number from the object
    push es
    call object_number_get
    mov [color], ax

    ; Check if the number is in range of possible values (between 0 and 15)
    push GRAPHICS_AMOUNT_COLORS
    push 0
    push ax
    call number_validate
    test ax, ax
    jz @@error_invalid_argument

    mov al, [color]
    mov [graphics_color], al
    jmp @@end_execute

@@error_arg_not_number:
    mov ax, [instruction_ptr]
    mov es, ax
    mov ax, [es:INSTRUCTION_ONE_ARG_OFF_ARG]
    mov es, ax
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_expected_number
    call runtime_error

@@error_invalid_argument:
    mov ax, [instruction_ptr]
    mov es, ax
    push [es:INSTRUCTION_OFF_FILE_INDEX]
    push offset error_message_invalid_argument_values
    call runtime_error

@@end_execute:
    push [arg_ptr]
    call object_deref

    pop es
    pop ax
    add sp, 3
    pop bp
    ret 2
endp instruction_setcolor_execute

; Execute a block segment given as a parameter
instruction_ptr = bp + 4
proc instruction_block_execute
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push es

    mov ax, [instruction_ptr]
    mov es, ax
    mov cx, [es:INSTRUCTION_BLOCK_OFF_LENGTH]
    mov ax, [es:INSTRUCTION_BLOCK_OFF_INSTRUCTIONS]
    mov es, ax

    mov bx, 0
@@loop_block:
    cmp cx, 0
    je @@end_loop_block

    ; Execute the current instruction
    push [es:bx]
    call instruction_execute

    dec cx
    add bx, 2
    jmp @@loop_block

@@end_loop_block:

    pop es
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
endp instruction_block_execute

instruction_ptr = bp + 4
write_x = bp - 2
write_y = bp - 4
proc instruction_setwritepos_execute
    push bp
    mov bp, sp
    sub sp, 4
    push ax
    push bx
    push dx
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    push [es:INSTRUCTION_ONE_ARG_OFF_ARG]
    call expr_eval
    mov [arg_ptr], ax
    mov es, ax ; Store the argument object segment
    ; Verify the object is a vector
    cmp [word ptr es:OBJECT_OFF_TYPE], offset object_vector_type
    jne @@error_arg_not_vector
    ; Get the actual coordinates from the vector object
    push es
    call object_vector_get_x
    mov [write_x], ax
    push es
    call object_vector_get_y
    mov [write_y], ax

    ; Check if number in range of screen width
    push [write_x]
    call graphics_validate_text_x
    jz @@error_invalid_argument
    ; Check if number in range of screen height
    push [write_y]
    call graphics_validate_text_y
    jz @@error_invalid_argument

    ; Save x and y in dl and dh respectively
    mov ax, [write_y]
    mov dh, al
    mov ax, [write_x]
    mov dl, al
    ; Ready up for interrupt
    mov ah, 2
    mov bh, 0
    int 10h
    jmp @@end_execute

@@error_arg_not_vector:
    mov ax, [instruction_ptr]
    mov es, ax
    mov ax, [es:INSTRUCTION_ONE_ARG_OFF_ARG]
    mov es, ax
    push [es:EXPR_OFF_FILE_INDEX]
    push offset error_message_expected_number
    call runtime_error

@@error_invalid_argument:
    mov ax, [instruction_ptr]
    mov es, ax
    push [es:INSTRUCTION_OFF_FILE_INDEX]
    push offset error_message_invalid_argument_values
    call runtime_error

@@end_execute:
    push [arg_ptr]
    call object_deref

    pop es
    pop dx
    pop bx
    pop ax
    add sp, 4
    pop bp
    ret 2
endp instruction_setwritepos_execute

; Exectue an instruction segment given as a parameter
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
    je @@choice_assign
    cmp al, INSTRUCTION_TYPE_IF
    je @@choice_if
    cmp al, INSTRUCTION_TYPE_LOOP
    je @@choice_loop
    cmp al, INSTRUCTION_TYPE_XLINE
    je @@choice_xline
    cmp al, INSTRUCTION_TYPE_YLINE
    je @@choice_yline
    cmp al, INSTRUCTION_TYPE_RECT
    je @@choice_rect
    cmp al, INSTRUCTION_TYPE_FILLEDRECT
    je @@choice_filledrect
    cmp al, INSTRUCTION_TYPE_DIAGONALLINE
    je @@choice_diagonalline
    cmp al, INSTRUCTION_TYPE_SHOW
    je @@choice_show
    cmp al, INSTRUCTION_TYPE_SETCOLOR
    je @@choice_setcolor
    cmp al, INSTRUCTION_TYPE_BLOCK
    je @@choice_block
    cmp al, INSTRUCTION_TYPE_SETWRITEPOS
    je @@choice_setwritepos

    ; If we matched nothing
    call panic

    jmp @@end_choice

@@choice_assign:
    mov ax, offset instruction_assign_execute
    jmp @@end_choice

@@choice_if:
    mov ax, offset instruction_if_execute
    jmp @@end_choice

@@choice_loop:
    mov ax, offset instruction_loop_execute
    jmp @@end_choice

@@choice_xline:
    mov ax, offset instruction_xline_execute
    jmp @@end_choice

@@choice_yline:
    mov ax, offset instruction_yline_execute
    jmp @@end_choice

@@choice_rect:
    mov ax, offset instruction_rect_execute
    jmp @@end_choice

@@choice_filledrect:
    mov ax, offset instruction_filledrect_execute
    jmp @@end_choice

@@choice_diagonalline:
    mov ax, offset instruction_diagonalline_execute
    jmp @@end_choice

@@choice_show:
    mov ax, offset instruction_show_execute
    jmp @@end_choice

@@choice_setcolor:
    mov ax, offset instruction_setcolor_execute
    jmp @@end_choice

@@choice_block:
    mov ax, offset instruction_block_execute
    jmp @@end_choice

@@choice_setwritepos:
    mov ax, offset instruction_setwritepos_execute

@@end_choice:

    ; Execute the instruction
    push es
    call ax

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_execute

; Execute all of the interpreter's instructions
proc interpreter_execute
    push ax
    push bx

    mov ax, 0
    lea bx, [parsed_instructions]
execute_instruction:
    cmp ax, [amount_instructions]
    je end_execute_instruction

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

; Set a variable in the interpreter
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

    ; Compare the key parameter with the key stored in the `variables` variable
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
    cmp [word ptr amount_variables], MAX_AMOUNT_VARIABLES
    je @@error_too_many_variables

    inc [word ptr amount_variables]
    mov ax, [key]
    mov [bx + 0], ax
    mov ax, [value]
    mov [bx + 2], ax
    jmp @@variable_was_set

@@error_too_many_variables:
    push offset error_message_too_many_variables
    call runtime_error_no_state

@@variable_was_set:

    pop cx
    pop bx
    pop ax
    pop bp
    ret 4
endp interpreter_set_variable

; Get a variable from the interpreter
name_ptr = bp + 4
proc interpreter_get_variable
    push bp
    mov bp, sp
    push bx
    push cx

    mov cx, 0
    lea bx, [variables]
@@find_variable:
    cmp cx, [word ptr amount_variables]
    je @@not_found_variable

    push [bx + 0]
    push [name_ptr]
    call cstrs_eq

    test ax, ax
    jnz @@found_variable

    inc cx
    add bx, 4
    jmp @@find_variable

@@found_variable:
    ; Get the object
    mov ax, [bx + 2]
    ; Add reference
    push ax
    call object_ref
    jmp @@end_find_variable

@@not_found_variable:
    mov ax, 0

@@end_find_variable:

    pop cx
    pop bx
    pop bp
    ret 2
endp interpreter_get_variable

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

    call wait_for_user_end_execution
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

    call wait_for_user_end_execution
    call exit_fail

    pop ax
    pop bp
    ret 4
endp runtime_error

; Remove data related to the interpreter
proc interpreter_delete
    push bx
    push cx

    ; Remove variables
    mov cx, 0
    lea bx, [variables]
@@loop_variables:
    cmp cx, [amount_variables]
    je @@end_loop_variables

    push [bx]
    call instruction_delete

    inc cx
    add bx, 2
    jmp @@loop_variables

@@end_loop_variables:

    pop cx
    pop bx
    ret
endp interpreter_delete

; Graphics related

; Convert a pair of x and y coordinates to an index we can later use to access the screen buffer
start_x = bp + 4
start_y = bp + 6
proc graphics_convert_position_to_index
    push bp
    mov bp, sp
    push bx
    push dx

    xor dx, dx
    mov ax, [start_y]
    mov bx, GRAPHICS_SCREEN_WIDTH
    mul bx
    add ax, [start_x]

    pop dx
    pop bx
    pop bp
    ret 4
endp graphics_convert_position_to_index

; Sets ax to 1 if x is inside the screen (0<=x<width) otherwise sets it to 0
x = bp + 4
proc graphics_validate_x
    push bp
    mov bp, sp

    push GRAPHICS_SCREEN_WIDTH
    push 0
    push [x]
    call number_validate

    pop bp
    ret 2
endp graphics_validate_x

; Sets ax to 1 if y is inside the screen (0<=y<height) otherwise sets it to 0
y = bp + 4
proc graphics_validate_y
    push bp
    mov bp, sp

    push GRAPHICS_SCREEN_HEIGHT
    push 0
    push [y]
    call number_validate

    pop bp
    ret 2
endp graphics_validate_y

; Returns into ax whether a start-size vector pair is possibly inside of the screen
start_x = bp + 4
start_y = bp + 6
size_width = bp + 8
size_height = bp + 10
proc graphics_validate_start_size_vectors
    push bp
    mov bp, sp

    ; Check if start x and end x are inside the screen
    push [start_x]
    call graphics_validate_x
    test ax, ax
    jz @@invalid_vectors
    mov ax, [start_x]
    add ax, [size_width]
    push ax
    call graphics_validate_x
    test ax, ax
    jz @@invalid_vectors

    ; Check if start y and end y are inside the screen
    push [start_y]
    call graphics_validate_y
    test ax, ax
    jz @@invalid_vectors
    mov ax, [start_y]
    add ax, [size_height]
    push ax
    call graphics_validate_y
    test ax, ax
    jz @@invalid_vectors

    ; If we got here, the vectors are valid
    mov ax, 1
    jmp @@end_validation

@@invalid_vectors:
    mov ax, 0

@@end_validation:

    pop bp
    ret 8
endp graphics_validate_start_size_vectors

; Validates the arguments for an `XLine` instructions
start_x = bp + 4
start_y = bp + 6
line_length = bp + 8
proc graphics_validate_xline
    push bp
    mov bp, sp

    push 0 ; Height
    push [line_length] ; Width
    push [start_y]
    push [start_x]
    call graphics_validate_start_size_vectors

    pop bp
    ret 6
endp graphics_validate_xline

; Validates the arguments for a `YLine` instruction
start_x = bp + 4
start_y = bp + 6
line_length = bp + 8
proc graphics_validate_yline
    push bp
    mov bp, sp

    push [line_length] ; Height
    push 0 ; Width
    push [start_y]
    push [start_x]
    call graphics_validate_start_size_vectors

    pop bp
    ret 6
endp graphics_validate_yline

; Sets ax to 1 if the argument is a valid x as a text mode index (0<=x<line width) otherwise sets it to 0
text_x = bp + 4
proc graphics_validate_text_x
    push bp
    mov bp, sp

    push GRAPHICS_SCREEN_LINE_WIDTH
    push 0
    push [text_x]
    call number_validate

    pop bp
    ret 2
endp graphics_validate_text_x

; Sets ax to 1 if the argument is a valid y as a text mode index (0<=y<amount lines) otherwise sets it to 0
text_y = bp + 4
proc graphics_validate_text_y
    push bp
    mov bp, sp

    push GRAPHICS_SCREEN_AMOUNT_LINES
    push 0
    push [text_y]
    call number_validate

    pop bp
    ret 2
endp graphics_validate_text_y

; Show horizontal line
start_x = bp + 4
start_y = bp + 6
line_length = bp + 8
line_direction = bp - 2
proc graphics_show_xline
    push bp
    mov bp, sp
    sub sp, 2
    push ax
    push bx
    push cx
    push es

    mov ax, 0A000h
    mov es, ax

    push [line_length]
    call number_get_direction
    mov [line_direction], ax ; Store direction for use in the loop

    push [start_y]
    push [start_x]
    call graphics_convert_position_to_index
    mov bx, ax

    mov cx, 0
@@loop_line:
    mov al, [graphics_color]
    mov [es:bx], al

    cmp cx, [line_length]
    je @@end_loop_line
    add cx, [line_direction]
    add bx, [line_direction]
    jmp @@loop_line

@@end_loop_line:

    pop es
    pop cx
    pop bx
    pop ax
    add sp, 2
    pop bp
    ret 6
endp graphics_show_xline

; Show vertical line
start_x = bp + 4
start_y = bp + 6
line_length = bp + 8
index_change = bp - 2
proc graphics_show_yline
    push bp
    mov bp, sp
    sub sp, 2
    push ax
    push bx
    push cx
    push dx
    push es

    mov ax, 0A000h
    mov es, ax

    ; Multiply sign by width to get the change in index we need to have each iteration
    push [line_length]
    call number_get_direction ; Returns to ax
    xor dx, dx
    mov bx, GRAPHICS_SCREEN_WIDTH
    mul bx
    mov [index_change], ax

    ; Make line_length absolute
    push [line_length]
    call number_abs
    mov [line_length], ax

    push [start_y]
    push [start_x]
    call graphics_convert_position_to_index
    mov bx, ax

    mov cx, 0
@@loop_line:
    mov al, [graphics_color]
    mov [es:bx], al

    cmp cx, [line_length]
    je @@end_loop_line
    inc cx
    add bx, [index_change]
    jmp @@loop_line

@@end_loop_line:

    pop es
    pop dx
    pop cx
    pop bx
    pop ax
    add sp, 2
    pop bp
    ret 6
endp graphics_show_yline

; Show a rectangle that's not filled
start_x = bp + 4
start_y = bp + 6
rect_width = bp + 8
rect_height = bp + 10
proc graphics_show_rect
    push bp
    mov bp, sp
    push ax

    ; Top
    push [rect_width]
    push [start_y]
    push [start_x]
    call graphics_show_xline
    ; Left
    push [rect_height]
    push [start_y]
    push [start_x]
    call graphics_show_yline

    ; Bottom
    push [rect_width]
    mov ax, [start_y]
    add ax, [rect_height]
    push ax ; We need this so we don't have an empty pixel in the bottom right corner
    push [start_x]
    call graphics_show_xline
    ; Right
    push [rect_height]
    push [start_y]
    mov ax, [start_x]
    add ax, [rect_width]
    push ax
    call graphics_show_yline

@@end_draw:

    pop ax
    pop bp
    ret 8
endp graphics_show_rect

; Show a filled rectangle
start_x = bp + 4
start_y = bp + 6
rect_width = bp + 8
rect_height = bp + 10
number_direction =  bp - 2
proc graphics_show_filledrect
    push bp
    mov bp, sp
    sub sp, 2
    push ax
    push cx

    ; Decide which function to call for better speed
    ; Less calls means less overhead although same amount of writes
    push [rect_width]
    call number_abs
    mov cx, ax
    push [rect_height]
    call number_abs
    cmp ax, cx ; Is abs(height) < abs(width)
    jb @@draw_height_times

    push [rect_width]
    call number_get_direction
    mov [number_direction], ax
    mov cx, 0
@@draw_vertical:
    push [rect_height]
    push [start_y]
    mov ax, [start_x]
    add ax, cx
    push ax
    call graphics_show_yline

    cmp cx, [rect_width]
    je @@end_draw
    add cx, [number_direction]
    jmp @@draw_vertical

@@draw_height_times:

    push [rect_height]
    call number_get_direction
    mov [number_direction], ax
    mov cx, 0
@@draw_horizontal:
    push [rect_width]
    mov ax, [start_y]
    add ax, cx
    push ax
    push [start_x]
    call graphics_show_xline

    cmp cx, [rect_height]
    je @@end_draw
    add cx, [number_direction]
    jmp @@draw_horizontal

@@end_draw:

    pop cx
    pop ax
    add sp, 2
    pop bp
    ret 8
endp graphics_show_filledrect

; Show a diagonal line
start_x = bp + 4
start_y = bp + 6
line_width = bp + 8
line_height = bp + 10
width_direction = bp - 2
height_direction = bp - 4
proc graphics_show_diagonalline
    push bp
    mov bp, sp
    sub sp, 4
    push ax
    push bx
    push cx
    push es

    cmp [word ptr line_width], 0
    jne @@non_zero
    cmp [word ptr line_height], 0
    jne @@non_zero

    ; If both width and height are 0, don't draw (drawing will result in a division by 0)
    jmp @@end_loop_draw

@@non_zero:

    mov ax, 0A000h
    mov es, ax

    ; Get direction of width
    push [line_width]
    call number_get_direction
    mov [width_direction], ax
    ; Get direction of height
    push [line_height]
    call number_get_direction
    mov [height_direction], ax
    ; Add to line width and height because we want to also color the last pixel
    mov ax, [width_direction]
    add [line_width], ax
    mov ax, [height_direction]
    add [line_height], ax

    ; Loop counter
    mov cx, 0

    ; Ax = abs(line_width), Bx = abs(line_height)
    push [line_height]
    call number_abs
    mov bx, ax
    push [line_width]
    call number_abs

    ; All of this is done in order to create a line with enough dots and minimize spacing between pixels
    ; Compare absolute width with absolute height
    cmp ax, bx
    ja @@loop_draw_width_times
    jmp @@loop_draw_height_times

@@loop_draw_width_times:
    cmp cx, [line_width]
    je @@end_loop_draw

    ; Calculate y of new point using the slope
    xor dx, dx
    mov ax, cx
    mov bx, [line_height]
    imul bx
    mov bx, [line_width]
    idiv bx
    ; Calculate position of new point as pixel
    xor dx, dx
    add ax, [start_y]
    mov bx, GRAPHICS_SCREEN_WIDTH
    imul bx
    add ax, [start_x]
    add ax, cx
    ; Move to bx for use as index
    mov bx, ax

    mov al, [graphics_color]
    mov [es:bx], al

    add cx, [width_direction]
    jmp @@loop_draw_width_times

@@loop_draw_height_times:
    cmp cx, [line_height]
    je @@end_loop_draw

    ; Calculate x of new point using the slope
    xor dx, dx
    mov ax, cx
    mov bx, [line_width]
    imul bx
    mov bx, [line_height]
    idiv bx
    push ax ; Store x of point
    ; Calculate position of new point as pixel
    xor dx, dx
    mov ax, [start_y]
    add ax, cx
    mov bx, GRAPHICS_SCREEN_WIDTH
    imul bx
    add ax, [start_x]
    pop bx ; Get back x of point and add it
    add ax, bx
    ; Move to bx for use as index
    mov bx, ax

    mov al, [graphics_color]
    mov [es:bx], al

    add cx, [height_direction]
    jmp @@loop_draw_height_times

@@end_loop_draw:

    pop es
    pop cx
    pop bx
    pop ax
    add sp, 4
    pop bp
    ret 8
endp graphics_show_diagonalline

start:
    mov ax, @data
    mov ds, ax

    ; Store the PSP segment for use when opening the file
    mov ax, es
    mov [PSP_segment], ax

    ; Set screen to graphic mode 320x200
    mov ah, 0
    mov al, 13h
    int 10h

    call open_file

    call parser_parse
    call interpreter_execute
    call interpreter_delete
    call parser_delete

    call close_file

    call wait_for_user_end_execution

    ; Set back to text mode
    mov ah, 0
    mov al, 03h
    int 10h

    ; Exit program gracefully (with exit code 0)
    mov ah, 4Ch
    mov al, 0
    int 21h
END start
