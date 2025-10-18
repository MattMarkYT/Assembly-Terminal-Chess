INCLUDE Irvine32.inc
INCLUDE SmallHelpers.inc

.code

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