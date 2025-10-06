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



; Symbols
SYM_KING    EQU "K"
SYM_QUEEN   EQU "Q"
SYM_KNIGHT  EQU "N"
SYM_BISHOP  EQU "B"
SYM_ROOK    EQU "R"
SYM_PAWN    EQU "p"
SYM_BLANK   EQU "-"

; Color - Used when printing the board
IS_BLACK    EQU 10000000b

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
INVALID_MOVE        EQU 1
MOVE_OUTSIDE_RANGE  EQU 2
KING_LEFT_IN_CHECK  EQU 3
EMPTY_SQUARE_MOVE   EQU 4
MISMATCH_COLOR      EQU 5
MISMATCH_COLOR_DEST EQU 6
BLOCKED_PATH        EQU 7
ERROR_PATH          EQU 8
ERROR_KING_CAPTURE  EQU 9


.data

    chess_title BYTE "Terminal Chess",0
    userInput BYTE INPUT_SIZE DUP(0)

    ; 4 left bits hold max number of jumps, 4 right bits hold move abilities
    KING_MOVE BYTE      00010011b
    QUEEN_MOVE BYTE     10000011b
    KNIGHT_MOVE BYTE    00010100b
    BISHOP_MOVE BYTE    10000001b
    ROOK_MOVE BYTE      10000010b
    PAWN_MOVE BYTE      00011000b

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
    mov eax, 0
    mov ebx, 0
    mov ecx, 0
    mov edx, 0
    call GetChessInput
    call ProcessInput   ; Turns input coords into range from 0-7
    mov al, FEEDBACK    
    test al, 0FFh       
    jnz StartTurn       ; If ProcessInput set a FEEDBACK flag, go back

    call InputToMove
    mov al, FEEDBACK    
    test al, 0FFh
    jnz StartTurn       ; If InputToMove set a FEEDBACK flag, go back

    call VerifyMove
    mov al, FEEDBACK
    test al, 0FFh
    jnz StartTurn

    mov al, GAME_STATUS
    xor al, IS_BLACK
    mov GAME_STATUS, al
    jmp StartTurn
    
    ret
chess ENDP

InputToMove PROC
    push edx
    push ecx
    ; Get First Piece
    mov esi, OFFSET userInput
    mov edx, [esi]              ; Copies userInput (a2c4[0123] in memory -> 3c2a[3210] in edx)
    call MoveSPointerToSquare   ; Move esi to the coords in dx

    ; Check piece type
    mov al, [esi]               ; Get square info
    and al, 0Fh                 ; Isolate piece type
    cmp al, PAWN
    je PAWN_LOGIC
    cmp al, QUEEN
    je QUEEN_LOGIC
    cmp al, KNIGHT
    je KNIGHT_LOGIC
    cmp al, BISHOP
    je BISHOP_LOGIC
    cmp al, ROOK
    je ROOK_LOGIC
    cmp al, KING
    je KING_LOGIC

    mov FEEDBACK, EMPTY_SQUARE_MOVE
    jmp SKIP
    
    PAWN_LOGIC:
    mov bl, PAWN_MOVE
    and bl, 0Fh
    mov bh, PAWN_MOVE
    shr bh, 4
    jmp AFTER_COPY_LOGIC
    QUEEN_LOGIC:
    mov bl, QUEEN_MOVE
    and bl, 0Fh
    mov bh, QUEEN_MOVE
    shr bh, 4
    jmp AFTER_COPY_LOGIC
    KNIGHT_LOGIC:
    mov bl, KNIGHT_MOVE
    and bl, 0Fh
    mov bh, KNIGHT_MOVE
    shr bh, 4
    jmp AFTER_COPY_LOGIC
    BISHOP_LOGIC:
    mov bl, BISHOP_MOVE
    and bl, 0Fh
    mov bh, BISHOP_MOVE
    shr bh, 4
    jmp AFTER_COPY_LOGIC
    ROOK_LOGIC:
    mov bl, ROOK_MOVE
    and bl, 0Fh
    mov bh, ROOK_MOVE
    shr bh, 4
    jmp AFTER_COPY_LOGIC
    KING_LOGIC:
    mov bl, KING_MOVE
    and bl, 0Fh
    mov bh, KING_MOVE
    shr bh, 4
    
    AFTER_COPY_LOGIC:
    ; Check color of moving piece
    mov al, [esi]               ; Get square info
    mov ah, GAME_STATUS         ; Get Game Status
    and al, 80h                 ; Isolate piece's is_black bit
    and ah, 80h                 ; Isolate game status's is_black_turn bit
    xor al, ah                  ; Check if the color is the same
    jz SOURCE_CORRECT_COLOR     ; If same, continue

    mov FEEDBACK, MISMATCH_COLOR
    jmp SKIP

    SOURCE_CORRECT_COLOR:
    ; Check destination square
    ror edx, 16                 ; Switch dx to the destination square
    call MoveDPointerToSquare
    ror edx, 16                 ; Switch dx back to source square

    ; Check if there is a piece there
    mov al, [edi]               ; Get square info
    and al, 0Fh                 ; Isolate piece type
    cmp al, PAWN
    je CHECK_COLOR
    cmp al, QUEEN
    je CHECK_COLOR
    cmp al, KNIGHT
    je CHECK_COLOR
    cmp al, BISHOP
    je CHECK_COLOR
    cmp al, ROOK
    je CHECK_COLOR
    cmp al, KING
    je CHECK_COLOR

    jmp DONT_CHECK_COLOR

    CHECK_COLOR:
    ; ah already has game status's color bit
    mov al, [edi]
    and al, 80h                 ; Isolate piece's is_black bit
    xor al, ah                  ; Check if the color is the same
    jnz DONT_CHECK_COLOR        ; If different, continue

    mov FEEDBACK, MISMATCH_COLOR_DEST
    jmp SKIP

    DONT_CHECK_COLOR:
    call GetCoordsDifference    ; Get difference in coords. ax is normal, cx is abs value
    test bl, DIAGONAL
    jnz DIAGONAL_LOGIC
    DIAGONAL_RET:

    test bl, PARALLEL
    jnz PARALLEL_LOGIC
    PARALLEL_RET:

    test bl, LSHAPE
    jnz LSHAPE_LOGIC
    LSHAPE_RET:

    test bl, FORWARD_ONLY
    jnz FORWARD_ONLY_LOGIC
    FORWARD_ONLY_RET:

    mov FEEDBACK, INVALID_MOVE
    jmp SKIP

    DIAGONAL_LOGIC:
    ; Check if it's diagonal
    cmp cl, ch                  ; Compare change in x with change in y
    jne DIAGONAL_RET            ; If not equal, go back and check other move types

    ; normalize vectors (make them equal 1 but keep sign)
    sar al, 7
    sar ah, 7
    or al, 1
    or ah, 1
    call CheckPath      ; Check path
    jmp SKIP
    

    PARALLEL_LOGIC:
    ; To check if it's parallel, we want to know if one value is 0 and another is non 0
    ; Since cx holds the absolute value of the coord differences,
    ; I'm just gonna check if it's greater than 7, if not, rotate the bits and check again
    ; EX: 05 00h > 7 so rotate bits, now 00 05h <= 7

    cmp cx, 7
    jg FLIP             ; If cx > 7, go rotate bits
    
    jmp IS_PARALLEL

    FLIP:
    ror cx, 8           ; rotate bits
    cmp cx, 7
    ror cx, 8           ; undo in case of return
    jg PARALLEL_RET     ; If c > 7, go back and check other move types
    
    ror cx, 8           ; redo so non 0 is in cl (CheckPath needs this)
    IS_PARALLEL:
    ; normalize vectors conditionally
    test al, 0FFh
    jz SKIP_X           ; If al = 0, skip

    sar al, 7
    or al, 1

    SKIP_X:
    test ah, 0FFh       ; If ah = 0, skip
    jz SKIP_Y

    sar ah, 7
    or ah, 1

    SKIP_Y:
    call CheckPath      ; Check path
    jmp SKIP


    LSHAPE_LOGIC:
    ; I'm doing a similar trick to parallel
    cmp cx, 0201h
    jne FLIP2           ; If cx != 0201h, go rotate bits
    
    jmp IS_LSHAPE

    FLIP2:
    ror cx, 8           ; rotate bits
    cmp cx, 0201h
    ror cx, 8           ; undo in case of return
    jg LSHAPE_RET       ; If cx != 0201h, go back and check other move types
    
    mov cl, 1           ; cl = 1 (CheckPath needs this)
    IS_LSHAPE:

    call CheckPath      ; Check path
    jmp SKIP

    FORWARD_ONLY_LOGIC:
    jmp FORWARD_ONLY_RET

    SKIP:
    pop ecx
    pop edx
    ret
InputToMove ENDP

; takes the coords in edx and return the difference
; al is difference in x, cl is the absolute value
; ah is difference in x, ch is the absolute value
GetCoordsDifference PROC
    mov al, dl      ; Get starting x
    mov ah, dh      ; Get starting y
    ror edx, 16     ; change dx to destination coords
    sub al, dl      ; subtract x by destination x
    mov cl, al
    jg SKIP_FLIP_X  ; If result was greater than 0, skip negation
    neg cl

    SKIP_FLIP_X:
    sub ah, dh      ; subtract y by destination y
    mov ch, ah
    jg SKIP_FLIP_Y  ; If result was greater than 0, skip negation
    neg ch

    SKIP_FLIP_Y:
    ror edx, 16         ; change dx to back to source coords
    ret
GetCoordsDifference ENDP

; esi starting position, edi ending position, ah y jump direction, al x jump direction, cl number of jumps, bh max number of jumps for piece type
CheckPath PROC
    ; Since we did x1-x2 and y1-y2 before, we have to invert the signs
    ; But since visually rows are in inverse position than they are in the array,
    ; we don't flip the y jump direction (Look at MoveSPointerToSquare for more info)
    neg al

    push esi

    sal ah, 3           ; multiply row by 2^3 = 8
    cmp cl, bh
    jg MOVE_IS_INVALID  ; If # of jumps is greater than max, give feedback

    jmp LOOP_START      ; else skip feedback and continue

    MOVE_IS_INVALID:
    mov FEEDBACK, INVALID_MOVE
    jmp SKIP

    LOOP_START:

    movsx ebx, ah
    add esi, ebx        ; Move over row
    movsx ebx, al
    add esi, ebx        ; Move over column

    mov bl, [esi]       ; Get next square in path

    dec cl
    jz AFTER_LOOP       ; If we have no more jumps, go to end of loop

    test bl, 0Fh    
    jz LOOP_START               ; If not a piece, continue loop

    mov FEEDBACK, BLOCKED_PATH  ; else set feedback and end
    jmp SKIP

    AFTER_LOOP:

    cmp esi, edi    
    je KING_CHECK               ; If source and destination are equal, continue

    mov FEEDBACK, ERROR_PATH
    jmp SKIP

    KING_CHECK:
    mov al, [edi]               ; Get square info
    and al, 0Fh                 ; Isolate piece type
    cmp al, KING
    jne SKIP                    ; If not trying to capture king, continue

    mov FEEDBACK, ERROR_KING_CAPTURE

    SKIP:
    pop esi
    ret
CheckPath ENDP

; Confirms that the move is valid.
; If it is, do the move. if it isn't, produce feedback
VerifyMove PROC
    ; Make a copy of the chessboard
    push esi
    push edi

    mov esi, OFFSET CHESSBOARD
    mov edi, OFFSET CHESSBOARD_COPY

    mov cl, 64
    LOOP_START:
    mov ch, [esi]
    mov BYTE PTR [edi], ch

    inc esi
    inc edi
    dec cl
    jnz LOOP_START
    
    pop edi
    pop esi

    mov al, [esi]
    mov BYTE PTR [edi], al      ; Overwrite destination with moving piece
    mov BYTE PTR [esi], 0       ; Cover up tracks

    mov esi, OFFSET CHESSBOARD
    mov cl, 64
    
    CHECK_FOR_KING:
    mov al, [esi]
    and al, 0Fh
    cmp al, KING
    je IS_KING

    jmp SKIP_KING

    IS_KING:
    mov al, [esi]
    mov ah, GAME_STATUS
    and al, 80h
    and ah, 80h
    xor al, ah
    jnz SKIP_KING

    jmp END_KING_SEARCH
    
    SKIP_KING:
    inc esi
    dec cl
    jnz CHECK_FOR_KING

    END_KING_SEARCH:
    mov ebx, esi
    sub ebx, OFFSET CHESSBOARD
    and ebx, 00000111b          ; Isolate column
    
    mov dl, bl

    mov ebx, esi
    sub ebx, OFFSET CHESSBOARD
    and ebx, 11111000b          ; Isolate row
    shl ebx, 5                  ; Shift left to have number of rows in bh
    
    mov dh, bh
    ; flip row
    sub dh, 7
    neg dh

    mov [userInput+2], dl 
    mov [userInput+3], dh 
    ror edx, 16                 ; Rotate king position to the destination side of edx
    
    ; Now we're going to loop through all pieces and see if they have a valid path to the king
    ; Temp change to other color
    mov al, GAME_STATUS
    xor al, IS_BLACK
    mov GAME_STATUS, al

    mov esi, OFFSET CHESSBOARD
    mov dl, 7
    mov dh, 7
    
    CHECK_FOR_ATTACKER:
    mov [userInput], dl 
    mov [userInput+1], dh 
    call InputToMove
    mov cl, FEEDBACK
    cmp cl, ERROR_KING_CAPTURE
    je IS_ATTACKER

    mov FEEDBACK, 0
    dec dl
    jns CHECK_FOR_ATTACKER
    mov dl, 7
    dec dh
    jns CHECK_FOR_ATTACKER

    jmp END_ATTACKER_SEARCH

    IS_ATTACKER:
    mov FEEDBACK, KING_LEFT_IN_CHECK
    ; UNDO CHANGE
    mov esi, OFFSET CHESSBOARD
    mov edi, OFFSET CHESSBOARD_COPY

    mov cl, 64
    UNDO_START:
    mov ch, [edi]
    mov BYTE PTR [esi], ch

    inc esi
    inc edi
    dec cl
    jnz UNDO_START
    jmp SKIP

    END_ATTACKER_SEARCH:
    mov FEEDBACK, 0

    SKIP:
    mov al, GAME_STATUS
    xor al, IS_BLACK
    mov GAME_STATUS, al
    ret
VerifyMove ENDP

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

    ;mov cl, 8
    ;mov esi, OFFSET [CHESSBOARD+8]

    PlaceBlackPawns:
    ;mov BYTE PTR [esi], BLACK_PAWN
    ;inc esi
    ;dec cl
    ;jnz PlaceBlackPawns

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
    ;mov BYTE PTR [esi], PAWN
    ;inc esi
    ;dec cl
    ;jnz PlaceWhitePawns

    ret
InitChessboard ENDP

; Takes chess input in dx (dh is row, dl is column)
; and sets esi to the square
MoveSPointerToSquare PROC
    push eax

    movsx eax, dh               ; Copy row

    ; Visually row 8 is actually the start of the array (If confused, look at chessboard from white's pov, then count left to right, up to down)
    ; so we have to invert the row number to get the row number in the array
    sub eax, 7
    neg eax
    shl eax, 3                  ; multiply row by 2^3 = 8
    add al, dl                  ; add column

    mov esi, OFFSET CHESSBOARD  ; Move source pointer to chessboard
    add esi, eax                ; Go to square

    pop eax
    ret
MoveSPointerToSquare ENDP

; Takes chess input in dx (dh is row, dl is column)
; and sets edi to the square
MoveDPointerToSquare PROC
    push eax
    movsx eax, dh               ; Copy row

    ; Refer to MoveSPointerToSquare just above this Procedure
    sub eax, 7
    neg eax
    shl eax, 3                  ; multiply row by 2^3 = 8
    add al, dl                  ; add column

    mov edi, OFFSET CHESSBOARD  ; Move source pointer to chessboard
    add edi, eax                ; Go to square

    pop eax
    ret
MoveDPointerToSquare ENDP

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
    add dh, dl          ; Undo the last add at the end of the previous loop
    mov cl, 8
    mov al, dh          ; White's view, dh = 0. Black's view, dh = 8
    add al, 96          ; 'a' - 1
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

    ; Set Text color to red
    mov eax, red + (black*16)
    call SetTextColor

    mov al, FEEDBACK
    test al, 0FFh
    jz END_OF_FEEDBACK

    cmp al, INVALID_MOVE
    je INVALID_MOVE_F

    cmp al, MISMATCH_COLOR
    je MISMATCH_COLOR_F

    cmp al, MISMATCH_COLOR_DEST
    je MISMATCH_COLOR_DEST_F

    cmp al, MOVE_OUTSIDE_RANGE
    je OUTSIDE_RANGE

    cmp al, BLOCKED_PATH
    je BLOCKED_PATH_F

    cmp al, KING_LEFT_IN_CHECK
    je KING_IN_CHECK

    cmp al, EMPTY_SQUARE_MOVE
    je E_SQUARE

    cmp al, ERROR_PATH
    je ERROR_PATH_F

    cmp al, ERROR_KING_CAPTURE
    je ERROR_KING_CAPTURE_F
    
    jmp END_OF_FEEDBACK

    INVALID_MOVE_F:
    mWriteln <"Invalid Move!">
    jmp END_OF_FEEDBACK

    MISMATCH_COLOR_F:
    mWriteLn <"Can't move piece that's not yours!">
    jmp END_OF_FEEDBACK

    MISMATCH_COLOR_DEST_F:
    mWriteLn <"You already occupying the end square!">
    jmp END_OF_FEEDBACK

    OUTSIDE_RANGE:
    mWriteln <"Invalid Syntax / Outside Range!">
    jmp END_OF_FEEDBACK

    KING_IN_CHECK:
    mWriteln <"Your king is left in check! ">
    jmp END_OF_FEEDBACK

    E_SQUARE:
    mWriteln <"Starting square is empty!">
    jmp END_OF_FEEDBACK

    BLOCKED_PATH_F:
    mWriteln <"Path between start and end is blocked by a piece!">
    jmp END_OF_FEEDBACK

    ERROR_KING_CAPTURE_F:
    mWriteln <"You should never be able to capture the king. That's a bug">
    jmp END_OF_FEEDBACK

    ERROR_PATH_F:
    mWriteln <"A bug occurred when checking the path between pieces">


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