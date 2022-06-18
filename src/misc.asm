DATASEG
    ; Panic related stuff
    panic_message db "* PANIC *", 0

CODESEG
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
    call prepare_for_exit
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

; Close file if opened, call parser_delete and call interpreter_delete
proc prepare_for_exit
    call interpreter_delete
    call parser_delete

    cmp [byte ptr have_file], 0
    je @@no_file

    call close_file

@@no_file:
    call wait_for_user_end_execution

    ret
endp prepare_for_exit

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
