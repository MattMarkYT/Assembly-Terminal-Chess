; Author: Matthew Marquez
; Terminal Chess written in assembly

include Irvine32.inc
include Macros.inc

; PIECE INFORMATION

; The pieces on the board are stored as BYTES
; The 2 left-most bit signifies BLACK/WHITE/EMPTY
; The next 2 bits signify if the piece has moved once and twice respectively
; The 4 right-most bits signify the PIECE TYPE

INPUT_SIZE EQU 5

; Color - Used when printing the board
IS_BLACK    EQU 10000000b
; Default color is used when both bits are 0

; Symbols
SYM_KING    EQU "K"
SYM_QUEEN   EQU "Q"
SYM_KNIGHT  EQU "N"
SYM_BISHOP  EQU "B"
SYM_ROOK    EQU "R"
SYM_PAWN    EQU "p"
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

; FEEDBACK
INVALID_MOVE        EQU 00000001b
MOVE_OUTSIDE_RANGE  EQU 00000010b
KING_LEFT_IN_CHECK  EQU 00000100b


.data

    chess_title BYTE "Terminal Chess",0
    userInput BYTE INPUT_SIZE DUP(0)

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
    FEEDBACK BYTE 0
    FIFTY_MOVE_RULE BYTE 0


    CHESSBOARD BYTE 64 DUP(0)
    CHESSBOARD_COPY BYTE 64 DUP(0)

.code

main PROC
    invoke SetConsoleTitle, OFFSET chess_title
    PLAY_AGAIN:
    call chess

    INVALID_MENU:
    mwriteln<"Do you want to play again? [y/n]:  ">
    mov edx, OFFSET userInput
    mov ecx, sizeof userInput
    call ReadChar
    call YesOrNo
    jc PLAY_AGAIN   ; if answer was y
    jz QUIT_GAME    ; if answer was n

    ; else input is invalid
    mwriteln <"Invalid Input. Try again!">

    jmp INVALID_MENU

    QUIT_GAME:
    exit

main ENDP

.code

chess PROC

    call initChessboard

    StartTurn:
    mGotoxy 0,0
    call Clrscr
    call PrintChessboard
    call PrintWhoseTurn
    call PrintFeedback
    
    GET_INPUT:
    call GetChessInput
    call ProcessInput   ; Turns input coords into range from 0-7
    mov al, FEEDBACK    ; If ProcessInput got something out of range
    test al, 0FFh
    jnz StartTurn



    
    ret
chess ENDP

GetChessInput PROC
    mGotoxy 17,20

    mov edx, OFFSET userInput
    mov ecx, INPUT_SIZE
    call ReadString

    ret
GetChessInput ENDP

ProcessInput PROC
    ; Convert characters to decimal values
    ; Letter
    mov al, [userInput]
    sub al, 97
    jl RANGE_ERROR          ; Check if it goes below 0
    mov [userInput], al
    ; Number
    mov al, [userInput+1]
    sub al, 49
    jl RANGE_ERROR          ; Check if it goes below 0
    mov [userInput+1], al
    ; Letter
    mov al, [userInput+2]
    sub al, 97
    jl RANGE_ERROR          ; Check if it goes below 0
    mov [userInput+2], al
    ; Number
    mov al, [userInput+3]
    sub al, 49
    jl RANGE_ERROR          ; Check if it goes below 0
    mov [userInput+3], al


    ; Check if it is above 7
    mov al, [userInput]
    cmp al, 7
    jg RANGE_ERROR
    mov al, [userInput+1]
    cmp al, 7
    jg RANGE_ERROR
    mov al, [userInput+2]
    cmp al, 7
    jg RANGE_ERROR
    mov al, [userInput+3]
    cmp al, 7
    jg RANGE_ERROR

    jmp SKIP

    RANGE_ERROR:
    mov FEEDBACK, MOVE_OUTSIDE_RANGE

    SKIP:
    ret
ProcessInput ENDP

InitChessboard PROC

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

    ret
InitChessboard ENDP

PrintWhoseTurn PROC
    mov al, GAME_STATUS
    test al, IS_BLACK_TURN
    jnz BLACK_TURN

    mWriteln <"White's turn">
    jmp SKIP

    BLACK_TURN:
    mWriteln <"Black's turn">

    SKIP:
    mWriteln <"------------">
    mWriteln <"Enter your move: ">
    call crlf
    ret
PrintWhoseTurn ENDP

PrintChessboard PROC
    pushad

    mGotoxy 0, 0

    ; Loop through Chessboard
    mov cl, 8   ; columns
    mov ch, 8   ; rows
    mov dl, 1   ; incrementer for esi and row/column labels

    mov al, GAME_STATUS
    test al, IS_BLACK_TURN
    jnz PrintBlackTurn

    ; PrintWhiteTurn
    mov esi, OFFSET CHESSBOARD
    mov dh, 8   ; Row labels start at 8
    neg dl      ; and count down
    jmp RowStart

    PrintBlackTurn:
    mov esi, OFFSET CHESSBOARD+63
    mov dh, 1   ; Row labels start at 1 and count up


    ; print the row number
    RowStart:
    mov eax, white + (black*16) ; Set Text color to white
    call SetTextColor
    movzx eax, dh               ; Print Row Number
    call WriteDec
    mWrite <"   ">

    PieceStart:
    call PrintPiece

    movsx ebx, dl
    neg ebx
    add esi, ebx
    dec cl
    jnz PieceStart
    call crlf
    call crlf
    mov cl, 8
    add dh, dl
    dec ch
    jnz RowStart

    ; Reset Text Color
    mov eax, white + (black*16)
    call SetTextColor

    ; Print the column labels
    neg dl
    mov cl, 8
    mov al, dh  ; White's view, dh = 0. Black's view, dh = 8
    add al, 97  ; Ascii value for lowercase a
    mWrite <"x   ">
    ColumnLabel:
    call WriteChar
    mWrite <"   ">
    add al, dl
    dec cl
    jnz ColumnLabel

    call crlf
    call crlf

    popad
    ret
PrintChessboard ENDP

PrintPiece PROC
    push edx

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
    jmp PrintP
    PrintRook:
    mov dl, SYM_ROOK
    jmp PrintP
    PrintKnight:
    mov dl, SYM_KNIGHT
    jmp PrintP
    PrintBishop:
    mov dl, SYM_BISHOP
    jmp PrintP
    PrintQueen:
    mov dl, SYM_QUEEN
    jmp PrintP
    PrintKing:
    mov dl, SYM_KING

    PrintP:
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

    pop edx
    ret
PrintPiece ENDP

PrintFeedback PROC
    mov al, FEEDBACK
    test al, 0FFh
    jz END_OF_FEEDBACK

    test al, MOVE_OUTSIDE_RANGE
    jnz OUTSIDE_RANGE


    test al, INVALID_MOVE
    jnz INVALID_MOVE_F
    
    jmp END_OF_FEEDBACK

    OUTSIDE_RANGE:
    mov eax, red + (black*16)
    call SetTextColor
    mWriteln <"Invalid Syntax / Outside Range!">
    jmp END_OF_FEEDBACK
    INVALID_MOVE_F:
    mov eax, red + (black*16)
    call SetTextColor
    mWriteln <"Invalid Move!">


    END_OF_FEEDBACK:
    mov eax, white + (black*16)
    call SetTextColor
    mov FEEDBACK, 0
    ret
PrintFeedback ENDP


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
    ret 
YesorNo ENDP


END main