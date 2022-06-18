DATASEG
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

    ; Object offsets
    OBJECT_OFF_TYPE = 0
    OBJECT_OFF_REFCOUNT = 2

    OBJECT_MAX_SIZE = 8

CODESEG
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
