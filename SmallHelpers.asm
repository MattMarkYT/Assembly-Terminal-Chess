INCLUDE Irvine32.inc
INCLUDE SmallHelpers.inc

KING            EQU 01h
IS_BLACK_TURN   EQU 10000000b

.code

; Takes chess input in dx (dh is row, dl is column), esi starting address of chessboard
; and sets esi to the square
MoveSPointerToSquare PROC uses eax
    movsx eax, dh               ; Copy row

    ; Visually row 8 is actually the start of the array (If confused, look at chessboard from white's pov, the array goes from left to right, top to bottom)
    ; so we have to invert the row label to get the row number in the array
    sub eax, 7
    neg eax
    shl eax, 3                  ; multiply row by 2^3
    add al, dl                  ; add column

    add esi, eax                ; Go to square

    ret
MoveSPointerToSquare ENDP

; Takes chess input in dx (dh is row, dl is column), edi starting address of chessboard
; and sets edi to the square
MoveDPointerToSquare PROC uses eax
    movsx eax, dh               ; Copy row

    ; Refer to MoveSPointerToSquare just above this Procedure
    sub eax, 7
    neg eax
    shl eax, 3                  ; multiply row by 2^3 = 8
    add al, dl                  ; add column

    add edi, eax                ; Go to square

    ret
MoveDPointerToSquare ENDP

; Takes esi starting address of chessboard, ah as GAME_STATUS's IS_BLACK_TURN bit
; and sets dx to king square coords depending on GAME_STATUS
; dl: column, dh: row
MoveDXToKing PROC

    mov dl, 0
    mov dh, 7
    START:
    mov al, [esi]
    and al, 0Fh
    cmp al, KING
    je IS_KING

    jmp SKIP

    IS_KING:
    mov al, [esi]
    and al, IS_BLACK_TURN
    cmp al, ah
    jne SKIP

    jmp END_SEARCH
    
    SKIP:
    inc esi
    inc dl
    cmp dl, 8
    jl START
    mov dl, 0
    dec dh
    jmp START

    END_SEARCH:
    ret
MoveDXToKing ENDP

; Takes in al(x) and ah(y) and normalizes them
; Turns to 1 if positive, -1 if negative, 0 if zero
NormalizeVectors PROC
    test al, 0FFh
    jz SKIP_X           ; If al = 0, skip

        sar al, 7       ; Normalize x
        or al, 1

    SKIP_X:
    test ah, 0FFh
    jz SKIP_Y           ; If ah = 0, skip

        sar ah, 7       ; Normalize y
        or ah, 1

    SKIP_Y:
    ret
NormalizeVectors ENDP

; Written by Malcolm McCullough
; Modified by Matthew Marquez
; Takes al
; sets carry flag if al is 'y' or 'Y'
; sets zero flag if al is 'n' or 'N'
; Usage: After calling, check carry flag before zero flag
YesOrNo PROC
    pushad
    START:

    cmp al, 'y'
    je SETCARRY
    cmp al, 'Y'
    je SETCARRY

    cmp al, 'n'
    je SETZERO
    cmp al, 'N'
    je SETZERO
    
    or al,1     ; clear zero flag
    clc         ; clear carry flag
    jmp SKIP

    SETZERO:
    test al,0   ; set zero flag
    clc         ; clear carry flag
    jmp SKIP

    SETCARRY:
    stc         ; set carry flag

    SKIP:
    popad
    ret 
YesOrNo ENDP

; takes the coords in edx and returns the difference
; al is difference in x, cl is the absolute value
; ah is difference in y, ch is the absolute value
GetCoordsDifference PROC
    mov al, dl          ; Get starting x
    mov ah, dh          ; Get starting y
    ror edx, 16         ; swap dx to destination coords
    sub al, dl          ; subtract x by destination x
    mov cl, al
    jg SKIP_FLIP_X      ; If result was greater than 0, don't negate cl
    neg cl

    SKIP_FLIP_X:
    sub ah, dh          ; subtract y by destination y
    mov ch, ah
    jg SKIP_FLIP_Y      ; If result was greater than 0, don't negate cl
    neg ch

    SKIP_FLIP_Y:
    ror edx, 16         ; swap dx to back to source coords
    ret
GetCoordsDifference ENDP

END