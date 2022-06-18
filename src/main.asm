IDEAL
MODEL small
STACK 300h

include 'src/misc.asm'
include 'src/error.asm'
include 'src/file.asm'
include 'src/parsing/struct/expr.asm'
include 'src/parsing/struct/instr.asm'
include 'src/parsing/token.asm'
include 'src/parsing/lexer.asm'
include 'src/parsing/parser.asm'
include 'src/objects/object.asm'
include 'src/objects/number.asm'
include 'src/objects/string.asm'
include 'src/objects/vector.asm'
include 'src/runtime/graphics.asm'
include 'src/runtime/interp.asm'

DATASEG
    ; Misc
    PSP_segment dw ?

CODESEG
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

    ; Remove everything and wait for a press on the Escape key
    call prepare_for_exit

    ; Set back to text mode
    mov ah, 0
    mov al, 03h
    int 10h

    ; Exit program gracefully (with exit code 0)
    mov ah, 4Ch
    mov al, 0
    int 21h
END start
