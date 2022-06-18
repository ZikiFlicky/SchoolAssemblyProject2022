DATASEG
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
    AMOUNT_PARSER_CMP_OPERATORS = 6

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

CODESEG
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
    call parser_error

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
