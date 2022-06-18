DATASEG
    ; File related
    have_file db 0
    file dw ? ; Stores handle
    file_idx dw 0 ; Stores index
    file_read_buffer db 40h dup(?) ; Small read buffer

CODESEG
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

    mov [byte ptr have_file], 1
    mov [file], ax

    pop es
    pop dx
    pop ax
    ret
endp open_file

; Closes our file
proc close_file
    push ax
    push bx

    ; Interrupt for closing files
    mov ah, 3Eh
    mov bx, [file]
    int 21h

    pop bx
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
