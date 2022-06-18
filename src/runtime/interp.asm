DATASEG
    ; Interpreter variables
    MAX_AMOUNT_VARIABLES = 20h
    variables dw MAX_AMOUNT_VARIABLES * 2 dup(?) ; Stores cstr-object pairs
    amount_variables dw 0
    ; Interpreter currently used color
    graphics_color db 0Fh ; Start color is white

CODESEG
; Expr evaluation functions

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

; Execute the `XLine` code instruction
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

; Execute the `YLine` code instruction
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

; Execute the `Rect` code instruction
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

; Execute the `FilledRect` code instruction
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

; Execute the `DiagonalLine` code instruction
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

; Execute the `Show` code instruction
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

; Execute the `SetColor` code instruction
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

; Execute an instruction segment given as a parameter
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
