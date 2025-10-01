; Author: Matthew Marquez
; Terminal Chess written in assembly

include Irvine32.inc
include Macros.inc

; PIECE INFORMATION

; The pieces on the board are stored as BYTES
; The 2 left-most bit signifies BLACK/WHITE/EMPTY
; The next 2 bits signify if the piece has moved once and twice respectively
; The 4 right-most bits signify the PIECE TYPE

; Color - Used when printing the board
IS_BLACK    EQU 10000000b
; Default color is used when both bits are 0

; Symbols
SYM_KING    EQU "K"
SYM_QUEEN   EQU "Q"
SYM_KNIGHT  EQU "N"
SYM_BISHOP  EQU "B"
SYM_ROOK    EQU "R"
SYM_PAWN    EQU "P"
SYM_BLANK   EQU "-"

; Piece Types
KING    EQU 01h
QUEEN   EQU 02h
KNIGHT  EQU 03h
BISHOP  EQU 04h
ROOK    EQU 05h
PAWN    EQU 06h

; Special Conditions
HAS_MOVED           EQU 00100000b
IS_EN_PASSANTABLE   EQU 00010000b

; Movement Abilities
DIAGONAL        EQU 00000001b
PARALLEL        EQU 00000010b
LSHAPE          EQU 00000100b
FORWARD_ONLY    EQU 00001000b

; Default Black Pieces
BLACK_KING      EQU 81h
BLACK_QUEEN     EQU 82h
BLACK_KNIGHT    EQU 83h
BLACK_BISHOP    EQU 84h
BLACK_ROOK      EQU 85h
BLACK_PAWN      EQU 86h

; Game Status Flags
BLACK_WON       EQU 00000001b
GAMEOVER        EQU 00000010b
IS_IN_CHECK     EQU 00001000b
IS_BLACK_TURN   EQU 10000000b


.data

    userInput BYTE 6 DUP(0)

Piece STRUCT
    jumpDist BYTE ?         ; Hex - 2 hex digits that control how far one jump is
    moveAbilities BYTE ?    ; Binary - 4 left bits hold max number of jumps, 4 right bits hold move abilities
Piece ENDS

    KING_MOVE Piece     <11h, 00010011b> 
    QUEEN_MOVE Piece    <11h, 10000011b> 
    KNIGHT_MOVE Piece   <12h, 00010100b> 
    BISHOP_MOVE Piece   <11h, 10000001b> 
    ROOK_MOVE Piece     <12h, 10000010b> 
    PAWN_MOVE Piece     <01h, 00011000b>

    GAME_STATUS BYTE 0
    FIFTY_MOVE_RULE BYTE 0


    CHESSBOARD BYTE 64 DUP(0)
    CHESSBOARD_COPY BYTE 64 DUP(0)

.code

main PROC
    AGAIN:
    call chess

    INVALID:
    mwriteln<"Do you want to play again? [y/n]:  ">
    mov edx, OFFSET userInput
    mov ecx, sizeof userInput
    call ReadChar
    call YesOrNo
    jc AGAIN    ; if answer was y
    jz SKIP     ; if answer was n

    ; else input is invalid
    mwriteln <"Invalid Input. Try again!">

    jmp INVALID

    SKIP:
    exit

main ENDP

.code

chess PROC
    ; Init Console
    mov eax, white + (black*16)
    call SetTextColor
    call Clrscr

    call initChessboard

    StartTurn:




    
    ret
chess ENDP

initChessboard PROC

    ; BLACK
    mov [CHESSBOARD+0], BLACK_ROOK
    mov [CHESSBOARD+1], BLACK_KNIGHT
    mov [CHESSBOARD+2], BLACK_BISHOP
    mov [CHESSBOARD+3], BLACK_QUEEN
    mov [CHESSBOARD+4], BLACK_KING
    mov [CHESSBOARD+5], BLACK_BISHOP
    mov [CHESSBOARD+6], BLACK_KNIGHT
    mov [CHESSBOARD+7], BLACK_ROOK

    mov cl, 8
    mov esi, OFFSET [CHESSBOARD+8]

    PlaceBlackPawns:
    mov BYTE PTR [esi], BLACK_PAWN
    inc esi
    dec cl
    jnz PlaceBlackPawns

    ; WHITE
    mov [CHESSBOARD+56], ROOK
    mov [CHESSBOARD+57], KNIGHT
    mov [CHESSBOARD+58], BISHOP
    mov [CHESSBOARD+59], QUEEN
    mov [CHESSBOARD+60], KING
    mov [CHESSBOARD+61], BISHOP
    mov [CHESSBOARD+62], KNIGHT
    mov [CHESSBOARD+63], ROOK

    mov cl, 8
    mov esi, OFFSET [CHESSBOARD+48]

    PlaceWhitePawns:
    mov BYTE PTR [esi], PAWN
    inc esi
    dec cl
    jnz PlaceWhitePawns

    call printChessboard

    ret
initChessboard ENDP

printChessboard PROC
    pushad

    mGotoxy 0, 0

    ; Loop through Chessboard
    mov cl, 8
    mov ch, 8
    mov esi, OFFSET CHESSBOARD

    RowStart:
    ; Check which piece type is on the square
    mov al, BYTE PTR [esi]
    and al, PAWN
    cmp al, PAWN
    je PrintPawn            ; If Pawn
    mov al, BYTE PTR [esi]
    and al, ROOK
    cmp al, ROOK
    je PrintRook            ; If Rook
    mov al, BYTE PTR [esi]
    and al, KNIGHT
    cmp al, KNIGHT
    je PrintKnight          ; If Knight
    mov al, BYTE PTR [esi]
    and al, BISHOP
    cmp al, BISHOP
    je PrintBishop          ; If Bishop
    mov al, BYTE PTR [esi]
    and al, QUEEN
    cmp al, QUEEN
    je PrintQueen           ; If Queen
    mov al, BYTE PTR [esi]
    and al, KING
    cmp al, KING
    je PrintKing            ; If King

    mov dl, SYM_BLANK
    jmp ColorDefault        ; Else

    ; Save character to DL
    PrintPawn:
    mov dl, SYM_PAWN
    jmp PrintPiece
    PrintRook:
    mov dl, SYM_ROOK
    jmp PrintPiece
    PrintKnight:
    mov dl, SYM_KNIGHT
    jmp PrintPiece
    PrintBishop:
    mov dl, SYM_BISHOP
    jmp PrintPiece
    PrintQueen:
    mov dl, SYM_QUEEN
    jmp PrintPiece
    PrintKing:
    mov dl, SYM_KING

    PrintPiece:
    ; Check the color of square
    mov al, BYTE PTR [esi]
    and al, 10000000b
    cmp al, IS_BLACK
    je ColorChangeToBlack
    jne ColorChangeToWhite

    ; Set Color
    ColorChangeToWhite:
    mov eax, yellow + (black*16)
    jmp SetPrintColor
    ColorChangeToBlack:
    mov eax, red + (black*16)
    jmp SetPrintColor
    ColorDefault:
    mov eax, white + (black*16)

    SetPrintColor:
    call SetTextColor

    ; Set stored character to al
    mov al, dl
    call WriteChar
    mWrite <"   ">

    inc esi
    dec cl
    jnz RowStart
    call crlf
    call crlf
    mov cl, 8
    dec ch
    jnz RowStart

    ; Reset Text Color
    mov eax, white + (black*16)
    call SetTextColor

    popad
    ret
printChessboard ENDP



; HELPER PROCEDURES

; Written by Malcolm McCullough
; Modified by Matthew Marquez
; takes al
; sets carry flag 
;  if  AL  is 'y' or 'Y'
; sets zero flag
;  if  AL  is 'n' or 'N'
YesOrNo PROC uses eax ebx ecx edx esi edi
    START:

    cmp al, 'y'
    je SETCARRY
    cmp al, 'Y'
    je SETCARRY

    cmp al, 'n'
    je SETZERO
    cmp al, 'N'
    je SETZERO
    
    or al,1   ; clear zero flag
    clc         ; clear carry flag
    jmp SKIP

    SETZERO:
    test al,0   ; set zero flag
    clc         ; clear carry flag
    jmp SKIP

    SETCARRY:
    stc         ; set carry flag

    SKIP:
    ret 
YesorNo ENDP


END main