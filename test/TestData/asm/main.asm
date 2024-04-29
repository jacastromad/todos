; There are 6 valid TODO comments in this file (a, b, c, d, e, f)
;TODO: a
;

; TODO b
section .text
    global _start

section .data
msg db  'Hello!'
len equ $ - msg

;	Todo !!! c

;    TODO: d

section .text

_start:        ; TODO: e

    mov edx,len
    mov ecx,msg
    mov ebx,1
    mov eax,4
    int 0x80

    mov ebx,0
    mov eax,1
    int 0x80

; todo f
