DATASEG
    GRAPHICS_SCREEN_WIDTH = 320
    GRAPHICS_SCREEN_HEIGHT = 200

    GRAPHICS_SCREEN_LINE_WIDTH = 40
    GRAPHICS_SCREEN_AMOUNT_LINES = 24

    GRAPHICS_AMOUNT_COLORS = 16 ; We have 16 possible colors

CODESEG
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
