IDEAL
MODEL small
STACK 100h
DATASEG
    ; Token types
    TOKEN_TYPE_VAR = 1
    TOKEN_TYPE_EQU = 2
    TOKEN_TYPE_PLUS = 3
    TOKEN_TYPE_NUMBER = 4
    TOKEN_TYPE_NEWLINE = 5
    TOKEN_TYPE_SHOW = 6

    ; Token offsets
    TOKEN_OFF_TYPE = 0
    TOKEN_OFF_START = 1
    TOKEN_OFF_LENGTH = 3

    TOKEN_SIZE = 5

    ; Instruction types
    INSTRUCTION_TYPE_ASSIGN = 1
    INSTRUCTION_TYPE_SHOW = 2

    ; Instruction offsets
    INSTRUCTION_OFF_TYPE = 0

    INSTRUCTION_ASSIGN_OFF_KEY = 1
    INSTRUCTION_ASSIGN_OFF_NUMBER = 3

    INSTRUCTION_SHOW_OFF_NUMBER = 1

    INSTRUCTION_MAX_SIZE = 5

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
    keywords dw offset keyword_show
    amount_keywords db 1

    ; Lexer error related stuff
    lexer_error_start db "LexerError: $"
    lexer_error_invalid_token db "Invalid token$"

    ; Parser error related stuff
    parser_error_start db "ParserError: $"
    parser_error_invalid_token db "Invalid token$"
    parser_error_expected_newline db "Expected newline$"
    ; Panic related stuff
    panic_message db "* PANIC *", 13, 10, "$" ; TODO: Highlighting here doesn't work, report bug

    ; FIXME: This can overflow + only allows a certain amount of instructions
    parsed_instructions dw 10h dup(?)
    amount_instructions dw 0
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

    pop bx
    pop bp
    ret 2
endp heap_alloc

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
start_idx = bp - 2
proc read_bytes
    push bp
    mov bp, sp
    sub sp, 2
    push bx
    push cx
    push dx

    mov ax, [file_idx]
    mov [start_idx], ax

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
    push [start_idx]
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
backtrack = bp - 2
times_newline = bp - 4
proc lex_newline
    push bp
    mov bp, sp
    sub sp, 4

    mov [token_type], TOKEN_TYPE_NEWLINE
    mov ax, [file_idx]
    mov [token_start_idx], ax ; The file index
    mov [word ptr token_length], 0

    mov [word ptr times_newline], 0

find_newline:
    ; Store backtrack
    mov ax, [file_idx]
    mov [backtrack], ax

    push 2
    call read_bytes
    test ax, ax
    jz not_found_newline ; You have to have 2 bytes for this

    cmp [byte ptr file_read_buffer], 13
    jne maybe_starts_with_linefeed
    cmp [byte ptr file_read_buffer + 1], 10
    jne not_found_newline

    jmp found_newline

maybe_starts_with_linefeed:
    cmp [byte ptr file_read_buffer], 10
    jne not_found_newline
    cmp [byte ptr file_read_buffer + 1], 13
    jne not_found_newline

found_newline:
    inc [word ptr times_newline]
    add [word ptr token_length], 2
    call remove_whitespace ; TODO: We should to add the length of the whitespace to the token length
    jmp find_newline

not_found_newline:
    push [backtrack]
    call file_set_idx
    cmp [word ptr times_newline], 0
    jg had_newlines

    mov ax, 0
    jmp finish_newline_lex

had_newlines:
    mov ax, 1

finish_newline_lex:

    add sp, 4
    pop bp
    ret
endp lex_newline

; Returns into ax a bool saying if we were able to lex the number
start_idx = bp - 2
proc lex_number
    push bp
    mov bp, sp
    sub sp, 2
    push bx

    mov ax, [file_idx]
    mov [start_idx], ax

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
    mov ax, [start_idx]
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
    push [start_idx]
    call file_set_idx
    mov ax, 0
    jmp end_number_lex

end_number_lex:

    pop bx
    add sp, 2
    pop bp
    ret
endp lex_number

; TODO: Speed can be improved when `keywords` is sorted, by storing information about max characters that are possible to read and minimum that don't have keyword char after them
backtrack = bp - 2
keyword_length = bp - 4
keyword_token_type = bp - 5
keyword_start_ptr = bp - 7
proc lex_keywords
    push bp
    mov bp, sp
    sub sp, 7
    push bx
    push cx

    mov ax, [file_idx]
    mov [backtrack], ax

    mov cx, 0
lex_single_keyword:
    mov al, [amount_keywords]
    cbw
    cmp cx, ax
    jge no_keyword_lex

    mov bx, cx
    shl bx, 1
    mov bx, [keywords + bx]
    mov al, [bx + 0]
    mov [keyword_token_type], al
    lea ax, [bx + 2]
    mov [keyword_start_ptr], ax
    mov al, [bx + 1]
    cbw
    mov [word ptr keyword_length], ax

    ; Check with one additional char
    inc ax
    push ax
    call read_bytes
    test ax, ax
    jz no_additional_keyword_char

    ; If we got here, we need to check if the last character is a keyword char or not
    lea bx, [file_read_buffer]
    mov ax, [keyword_length]
    add bx, ax
    mov al, [bx]
    cbw
    push ax
    call is_char_var ; Check if keyword char
    test ax, ax
    jnz keyword_no_match

    push [backtrack]
    call file_set_idx

no_additional_keyword_char:
    ; Try to read just enough data
    push [keyword_length]
    call read_bytes
    test ax, ax
    jz keyword_no_match

    ; NOTE: This loop forces us to have keywords that have a length greater than 0
    mov cx, 0
keyword_char_cmp:
    mov bx, [keyword_start_ptr]
    add bx, cx
    mov al, [bx]

    lea bx, [file_read_buffer]
    add bx, cx
    mov ah, [bx]

    cmp al, ah
    jne keyword_no_match

    inc cx
    cmp cx, [keyword_length]
    jl keyword_char_cmp

    ; If we got here, we matched the keyword
    mov al, [keyword_token_type]
    mov [token_type], al
    mov ax, [keyword_length]
    mov [token_length], ax
    mov ax, [backtrack]
    mov [token_start_idx], ax

    mov ax, 1
    jmp end_keyword_lexing

no_keyword_lex:
    mov ax, 0
    jmp end_keyword_lexing

keyword_no_match:

    push [backtrack]
    call file_set_idx
    inc cx
    jmp lex_single_keyword

end_keyword_lexing:

    pop cx
    pop bx
    add sp, 7
    pop bp
    ret
endp lex_keywords

; Returns into ax a bool saying if we were able to lex the variable name
start_idx = bp - 2
proc lex_var
    push bp
    mov bp, sp
    sub sp, 2

    mov ax, [file_idx]
    mov [start_idx], ax

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
    mov ax, [start_idx]
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
    push [start_idx]
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
start_idx = bp - 2
proc lex_char
    push bp
    mov bp, sp
    sub sp, 2

    mov ax, [file_idx]
    mov [start_idx], ax

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
    mov ax, [start_idx]
    mov [token_start_idx], ax
    mov [token_length], 1
    mov ax, 1
    jmp lex_char_end

lex_char_fail:
    push [start_idx]
    call file_set_idx
    mov ax, 0
lex_char_end:

    add sp, 2
    pop bp
    ret 2
endp lex_char

message_ptr = bp + 4
proc lexer_error
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    ; LexerError:
    mov ah, 09h
    lea dx, [byte ptr lexer_error_start]
    int 21h
    ; Some error message
    mov ah, 09h
    mov dx, [message_ptr]
    int 21h
    call print_newline

    pop dx
    pop cx
    pop bx
    pop ax
    pop bp
    ret 2
endp lexer_error

backtrack = bp - 2
proc remove_whitespace
    push bp
    mov bp, sp
    sub sp, 2
    push ax

keep_removing:
    mov ax, [file_idx]
    mov [backtrack], ax
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

start_idx = bp - 2
proc lex
    push bp
    mov bp, sp
    sub sp, 2

    call remove_whitespace

    mov ax, [file_idx]
    mov [start_idx], ax

    ; Check for EOF
    push 1
    call read_bytes
    push [start_idx]
    call file_set_idx
    test ax, ax
    jz lex_failed

    call lex_newline
    test ax, ax
    jnz end_lex

    call lex_number
    test ax, ax
    jnz end_lex

    call lex_keywords
    test ax, ax
    jnz end_lex

    call lex_var
    test ax, ax
    jnz end_lex

    mov ah, TOKEN_TYPE_EQU
    mov al, '='
    push ax
    call lex_char
    test ax, ax
    jnz end_lex

    mov ah, TOKEN_TYPE_PLUS
    mov al, '+'
    push ax
    call lex_char
    test ax, ax
    jnz end_lex

    ; FIXME: This should exit somewhere
    push offset lexer_error_invalid_token
    call lexer_error

lex_failed:
    mov ax, 0

end_lex:
    add sp, 2
    pop bp
    ret
endp lex

; Parser procedures

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

message_ptr = bp + 4
proc parser_error
    push bp
    mov bp, sp
    push ax
    push bx
    push cx
    push dx

    ; ParserError:
    mov ah, 09h
    lea dx, [byte ptr parser_error_start]
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
endp parser_error

; Returns into ax whether we matched a token type
expected_type = bp + 4
start_idx = bp - 2
proc parser_match
    push bp
    mov bp, sp
    sub sp, 2

    mov ax, [file_idx]
    mov [start_idx], ax

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
    push [start_idx]
    call file_set_idx
    mov ax, 0

end_match:

    add sp, 2
    pop bp
    ret 2
endp parser_match

key_token = bp - TOKEN_SIZE
value_token = bp - TOKEN_SIZE - TOKEN_SIZE
key_segment = bp - TOKEN_SIZE - TOKEN_SIZE - 2
number = bp - TOKEN_SIZE - TOKEN_SIZE - 2 - 2
proc parser_parse_assignment
    push bp
    mov bp, sp
    sub sp, TOKEN_SIZE + TOKEN_SIZE + 2 + 2
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
    jz assignment_error_no_equal_sign

    push TOKEN_TYPE_NUMBER
    call parser_match
    test ax, ax
    jz assignment_error_no_value

    ; Copy token into the given pointer
    lea ax, [value_token]
    push ax
    push ss
    call token_copy

    ; Set the global token to the key token
    lea ax, [key_token]
    push ax
    push ss
    call token_set
    ; Convert to cstr (NUL-terminated string)
    call token_to_cstr
    mov [key_segment], ax

    ; Set the global token to the value token
    lea ax, [value_token]
    push ax
    push ss
    call token_set
    ; Convert to number
    call token_to_number
    mov [number], ax

    ; Create new instruction with type Assign
    push INSTRUCTION_TYPE_ASSIGN
    call instruction_new

    ; Make es point to the new instruction segment
    mov es, ax
    ; Store the key in the instruction
    mov ax, [key_segment]
    mov [es:INSTRUCTION_ASSIGN_OFF_KEY], ax
    ; Store the number in the instruction
    mov ax, [number]
    mov [es:INSTRUCTION_ASSIGN_OFF_NUMBER], ax

    ; Move es back to ax so we can return the pointer to the instruction
    mov ax, es

    jmp end_parse_assignment

assignment_error_no_equal_sign:
assignment_error_no_value:
    push offset parser_error_invalid_token
    call parser_error
    jmp end_parse_assignment

not_parse_assignment:
    mov ax, 0

end_parse_assignment:

    pop es
    add sp, TOKEN_SIZE + TOKEN_SIZE + 2 + 2
    pop bp
    ret
endp parser_parse_assignment

proc parser_parse_show
    push es

    push TOKEN_TYPE_SHOW
    call parser_match
    test ax, ax
    jz not_parse_show

    push TOKEN_TYPE_NUMBER
    call parser_match
    test ax, ax
    jz show_error_no_value

    push INSTRUCTION_TYPE_SHOW
    call instruction_new
    mov es, ax

    ; Convert the number token into a value
    call token_to_number

    mov [es:INSTRUCTION_SHOW_OFF_NUMBER], ax

    mov ax, es
    jmp end_parse_show

show_error_no_value:
    push offset parser_error_invalid_token
    call parser_error
    jmp end_parse_show

not_parse_show:
    mov ax, 0

end_parse_show:

    pop es
    ret
endp parser_parse_show

; TODO: Maybe make this return a bool, allowing us to not exit in parser_error?
backtrack = bp - 2
proc expect_newline
    push bp
    mov bp, sp
    sub sp, 2 ; Allocate space for backtrack
    push ax

    ; Store backtrack
    mov ax, [file_idx]
    mov [backtrack], ax

    call lex
    test ax, ax
    jz newline_reached ; If we couldn't lex, this is the end of the file

    cmp [byte ptr token_type], TOKEN_TYPE_NEWLINE
    je newline_reached

    ; If we got here, we should backtrack to the beggining of the token and error
    push [backtrack]
    call file_set_idx
    push offset parser_error_expected_newline
    call parser_error

newline_reached:

    pop ax
    add sp, 2
    pop bp
    ret
endp expect_newline

; FIXME: Check if it's okay to return 0 or is it possibly an allocated segment (not in code)
; Returns a parsed instruction segment into ax, or 0 if not found
proc parser_parse_instruction
    call parser_parse_assignment
    test ax, ax
    jnz parsed_instruction
    call parser_parse_show
    test ax, ax
    jnz parsed_instruction

    ; If wasn't able to parse an instruction, return a NULL
    mov ax, 0
    jmp end_parse_instruction

parsed_instruction:
    call expect_newline

end_parse_instruction:

    ret
endp parser_parse_instruction

; Parse all of the file
proc parser_parse
    push ax
    push bx

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

instruction_parsing_failed:
    ; TODO: Check here if you have a remainder of non-parsable code
    ; call panic

    pop bx
    pop ax
    ret
endp parser_parse

; Interpreter procedures

instruction_ptr = bp + 4
proc instruction_assign_execute
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    push [es:INSTRUCTION_ASSIGN_OFF_NUMBER] ; value
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

    push [es:INSTRUCTION_SHOW_OFF_NUMBER] ; value
    call print_word
    call print_newline

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_show_execute

instruction_ptr = bp + 4
proc instruction_execute
    push bp
    mov bp, sp
    push ax
    push es

    mov ax, [instruction_ptr]
    mov es, ax

    mov ah, [es:INSTRUCTION_OFF_TYPE]

    cmp ah, INSTRUCTION_TYPE_ASSIGN
    je choice_instruction_assign
    cmp ah, INSTRUCTION_TYPE_SHOW
    je choice_instruction_show

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

end_instruction_choice:

    pop es
    pop ax
    pop bp
    ret 2
endp instruction_execute

proc interpreter_execute
    push ax
    push bx

    mov ax, 0
execute_instruction:
    cmp ax, [amount_instructions]
    jge end_execute_instruction

    mov bx, ax
    shl bx, 1 ; Mul by 2 to align as words (because we store pointers to instructions)
    add bx, offset parsed_instructions ; Add base to the offset to have base+index

    push [bx]
    call instruction_execute

    inc ax
    jmp execute_instruction

end_execute_instruction:

    pop bx
    pop ax
    ret
endp interpreter_execute

; TODO: Add real functionality of setting values
key = bp + 4
value = bp + 6
proc interpreter_set_variable
    push bp
    mov bp, sp
    push ax
    push dx

    push ds ; NOTE: This behavior isn't that great because we might need to use global data-segment variables in procedures
    mov ax, [key]
    mov ds, ax
    push 0 ; Offset is always zero to segment-aligned heap blocks
    call print_nul_terminated_string
    pop ds
    ; Show newline
    call print_newline

    pop dx
    pop ax
    pop bp
    ret 4
endp interpreter_set_variable

start:
    mov ax, @data
    mov ds, ax

    call open_file
; relex:
;     call lex
;     test ax, ax
;     jne relex

    call parser_parse
    call interpreter_execute

    ; exit
    mov ax, 4C00h
    int 21h
END start
