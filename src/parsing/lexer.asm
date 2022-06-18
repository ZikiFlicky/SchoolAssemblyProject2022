DATASEG
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

CODESEG
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
