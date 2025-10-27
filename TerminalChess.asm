; Author: Matthew Marquez
; Terminal Chess written in assembly

include Irvine32.inc
include Macros.inc

include SmallHelpers.inc
include ChessFeedback.inc

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
NO_EN_PASSANT       EQU 11101111b

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
GAMEOVER        EQU 00000001b
STALEMATE       EQU 00000010b
IS_BLACK_TURN   EQU 10000000b


.data

    chess_title BYTE "Terminal Chess",0
    userInput BYTE INPUT_SIZE DUP(0)

    ; 4 left bits hold max number of jumps, 4 right bits hold move abilities (diagonal, parallel, etc)
    KING_MOVE BYTE      00010011b
    QUEEN_MOVE BYTE     10000011b
    KNIGHT_MOVE BYTE    00010100b
    BISHOP_MOVE BYTE    10000001b
    ROOK_MOVE BYTE      10000010b
    PAWN_MOVE BYTE      00101000b

    GAME_STATUS BYTE 0
    FIFTY_MOVE_RULE BYTE 0

    CHESSBOARD          BYTE 64 DUP(0)
    CHESSBOARD_COPY     BYTE 64 DUP (0)

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

chess PROC
    
    ; Initialize Chessboard
    call initChessboard

    StartTurn:
        call Clrscr             ; Clear Screen
        call PrintChessboard    ; Print Chessboard
        
        mov al, GAME_STATUS
        test al, GAMEOVER
        jnz WIN

        call PrintWhoseTurn     ; Print the "Enter your move" label
        PrintFeedback           ; Print feedback if any
    
    GET_INPUT:
        ; Clear registers
        mov eax, 0
        mov ebx, 0
        mov ecx, 0
        mov edx, 0
        mov esi, 0
        mov edi, 0
        call GetChessInput  ; Ask the player for input

        call ProcessInput   ; Turns ascii input in userInput into coords ranging from 0-7

        call InputToMove    ; Check if move from source to destination is valid
        GetFeedback         ; move FEEDBACK Byte into al
        cmp al, 0
        jne StartTurn       ; If FEEDBACK != 0, restart turn

        call VerifyMove     ; Second round of input validation (Leaving king in check, etc)
        GetFeedback         ; move FEEDBACK Byte into al
        cmp al, 0
        jne StartTurn       ; If FEEDBACK != 0, restart turn

    ; Switch turn
    mov al, GAME_STATUS
    xor al, IS_BLACK
    mov GAME_STATUS, al

    call CheckmateCondition

    jmp StartTurn           ; Loop back if game not over
    
    WIN:
    call PrintWinner
    ret
chess ENDP

CheckmateCondition PROC
    push [edi]
    push edi
    mov esi, OFFSET CHESSBOARD
    mov ah, GAME_STATUS
    and ah, IS_BLACK_TURN
    call MoveDXToKing

    call CanKingMove
    GetFeedback
    cmp al, 0                   ; No feedback == king can move
    je SKIP                     ; If king can move, not checkmate

    call CheckForSquareAttackers
    cmp cl, 1
    jg CHECKMATE                ; If king can't move and checked by > one piece, it is checkmate
    jl SKIP                     ; If checked by no piece, not checkmate

    ; else if checked by one piece
    mov edi, OFFSET CHESSBOARD
    call MoveDPointerToSquare
    ror edx, 16                 ; Swap King Coords to destination side (most sig half of edx)
    mov dx, bx                  ; Move Attacking piece coords to source side (least sig half of edx)
    
    mov esi, OFFSET CHESSBOARD
    call MoveSPointerToSquare
    call GetPieceLogic
    call GetCoordsDifference
    
    cmp bl, LSHAPE
    je SKIP_NORMALIZE
    
    call NormalizeVectors

    SKIP_NORMALIZE:
    call CheckPathForDefenders
    GetFeedback
    cmp al, 0
    je CHECKMATE

    jmp SKIP

    CHECKMATE:
    mov al, GAME_STATUS
    or al, GAMEOVER
    mov GAME_STATUS, al
    SKIP:
    pop edi
    pop [edi]
    ret
CheckmateCondition ENDP

; Used in checking a move is valid
InputToMove PROC uses edx ecx ebx
    SetFeedback 0
    ; Get First Piece
    mov edx, DWORD PTR [userInput]      ; Copies userInput (a2c4[0123] in memory -> 4c2a[3210] in edx)

    ; Check if source and destination coords are the same
    mov bx, dx                      ; Copy source coords to bx
    ror edx, 16                     ; Temp switch dx to the destination coords
    cmp bx, dx
    jne NotSameSquare               ; if source != destination, continue

    SetFeedback SAME_SQUARE         ; else set feedback and skip to the end
    jmp SKIP

    NotSameSquare:
    ; Check if source or destination coords are outside range
    cmp bl, 7
    ja OUTSIDE_RANGE
    cmp bh, 7
    ja OUTSIDE_RANGE
    cmp dl, 7
    ja OUTSIDE_RANGE
    cmp dh, 7
    ja OUTSIDE_RANGE

    jmp IN_RANGE

    OUTSIDE_RANGE:
    SetFeedback MOVE_OUTSIDE_RANGE
    jmp SKIP

    IN_RANGE:
    ; Getting the source square
    ror edx, 16                     ; Switch dx back to source square
    mov esi, OFFSET CHESSBOARD
    call MoveSPointerToSquare       ; Move esi to the source coords in dx

    ; Remove IS_EN_PASSANTABLE (Pawns can only be en passanted the move after, so we're removing the attribute)
    mov bl, [esi]
    and bl, NO_EN_PASSANT
    mov BYTE PTR [esi], bl

    ;   Series of Premove validations  ;
    
    ; Check if square is empty
    mov al, [esi]
    cmp al, 0
    jne SQUARE_NOT_EMPTY

    SetFeedback EMPTY_SQUARE_MOVE
    jmp SKIP

    SQUARE_NOT_EMPTY:
    ; Check if player owns the moving piece
    mov al, [esi]               ; Get square info
    mov ah, GAME_STATUS         ; Get Game Status
    and al, 80h                 ; Isolate piece's is_black bit
    and ah, 80h                 ; Isolate game status's is_black_turn bit
    xor al, ah                  ; Check if the color is the same
    jz SOURCE_CORRECT_COLOR     ; If same, continue

    SetFeedback MISMATCH_OWNER
    jmp SKIP

    SOURCE_CORRECT_COLOR:
    ; Getting the destination square
    ror edx, 16                 ; Switch dx to the destination square
    mov edi, OFFSET CHESSBOARD
    call MoveDPointerToSquare
    ror edx, 16                 ; Switch dx back to source square
    ; Check if there is a piece there
    mov al, [edi]               ; Get destination square info
    cmp al, 0
    jne CHECK_COLOR             ; If al != 0 (not empty), check color

    jmp DONT_CHECK_COLOR        ; else don't check color

    CHECK_COLOR:
    ; Check if capturing piece is the player's
    ; (Note: ah already has game status's color bit from earlier)
    mov al, [edi]                   ; Get destination square info
    and al, 80h                     ; Isolate is_black bit
    xor al, ah                      ; Check if the color is the same
    jnz DONT_CHECK_COLOR            ; If different, continue

    SetFeedback MISMATCH_COLOR_CAPT
    jmp SKIP

    DONT_CHECK_COLOR:
    ; Now we're going to do pathing validations
    ; Check piece type
    call GetPieceLogic
    GetFeedback    
    cmp al, 0
    jne SKIP

    call GetCoordsDifference    ; Get difference in coords. al(x) and ah(y) are normal, cl(x) and ch(y) are absolute value
    
    ; Check all of the piece's move abilities to see if it's a valid move
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

    ; If none found, invalid move
    SetFeedback INVALID_MOVE
    jmp SKIP


    ;   DIAGONAL LOGIC   ;
    ; To check if it's diagonal, we want to know if abs(x) == abs(y)
    DIAGONAL_LOGIC:

    cmp cl, ch
    jne DIAGONAL_RET        ; If move is not diagonal, go back and check other move types

    call NormalizeVectors   ; normalize ax vectors (make them either 1 or -1)
    call CheckPath          ; Check path
    jmp SKIP
    

    ;   PARALLEL LOGIC   ;
    ; To check if it's parallel, we want to know if either x or y is 0 and the other is non 0
    ; I'm just gonna check if cx greater than 7, if not, swap cl and ch and check again
    ; EX: 05 00h > 7 so rotate bits, now 00 05h <= 7
    PARALLEL_LOGIC:

    cmp cx, 7
    jg FLIP                 ; If cx > 7, go rotate bits
    
    jmp IS_PARALLEL

    FLIP:
    ror cx, 8               ; rotate bits
    cmp cx, 7
    ror cx, 8               ; undo in case of jump
    jg PARALLEL_RET         ; If c > 7, go back and check other move types
    
    ror cx, 8               ; redo so non 0 is in cl (CheckPath needs this)
    IS_PARALLEL:
    call NormalizeVectors   ; normalize ax vectors
    call CheckPath          ; Check path
    jmp SKIP


    ;   LSHAPE LOGIC   ;
    ; To check if it's L-shaped, we want to know if x and y have a 2:1 ratio
    ; To-Do: Make this work with moves that are greater than 0201h (Ex: 0402h)
    LSHAPE_LOGIC:
    ; I'm doing a similar trick to parallel
    cmp cx, 0201h
    jne FLIP2           ; If cx doesn't have 2:1 ratio, go rotate bits
    
    jmp IS_LSHAPE

    FLIP2:
    ror cx, 8           ; rotate bits
    cmp cx, 0201h
    ror cx, 8           ; undo in case of jump
    jne LSHAPE_RET      ; If cx != 0201h, go back and check other move types
    
    ror cx, 8           ; redo so lesser value is in cl (CheckPath needs this)
    IS_LSHAPE:

    call CheckPath      ; Check path
    jmp SKIP


    ;   FORWARD ONLY LOGIC   ;
    ; To check if it's Forward Only, we want to know if y changes, but x is 0
    FORWARD_ONLY_LOGIC:
    ; Check for a single jump or a double jump
    test ch, 1
    jz CHECK_DOUBLE             ; If change in y != 1, check double jump

    ; else check if capture
    cmp cx, 0100h
    je CHECK_DIRECTION          ; If change in y == 1 AND change in y == 0, continue
    cmp cx, 0101h
    jne FORWARD_ONLY_RET        ; If change in y != 1 OR change in y != 1, go back and give feedback

    ; else check if valid capture
    mov dl, [edi]
    cmp dl, 0     
    jne CHECK_DIRECTION         ; If square not empty, continue (Normal capture)

    mov edi, esi                ; Move edi to esi
    movsx edx, al               
    neg edx
    add edi, edx                ; add change in x to edi (En passant checks for piece next to pawn)

    mov dl, [edi]
    test dl, IS_EN_PASSANTABLE
    jz FORWARD_ONLY_RET         ; If piece next to pawn can't be en passanted, go back and give feedback

    mov dl, [edi]               ; Get square info
    mov dh, GAME_STATUS         ; Get Game Status
    and dl, 80h                 ; Isolate piece's is_black bit
    and dh, 80h                 ; Isolate game status's is_black_turn bit
    xor dl, dh                  ; Check if the color is different
    jnz CHECK_DIRECTION         ; If color is different, continue
    
    jmp FORWARD_ONLY_RET        ; else go back and give feedback

    CHECK_DOUBLE:
    cmp cx, 0200h
    jne FORWARD_ONLY_RET        ; If change in y != 2 OR change in x != 0, go back and give feedback
    mov dl, [esi]
    test dl, HAS_MOVED
    jnz FORWARD_ONLY_RET        ; If has moved already, go back and give feedback

    CHECK_DIRECTION:
    mov dh, ah                  ; move ah to dh
    mov dl, GAME_STATUS
    and dl, 80h                 ; Isolate game status's IS_BLACK_TURN bit
    xor dh, GAME_STATUS         ; bit is 1 when correct direction
    and dh, IS_BLACK_TURN       ; Checks if bit is 1 or 0
    jz FORWARD_ONLY_RET         ; If incorrect direction, go back and give feedback

    ; Final check, if move isn't diagonal and there's a piece ahead, don't capture
    cmp cl, 0
    jne F_ONLY_CONTINUE         ; If horizontal movement not 0, continue

    ; else check destination
    mov dl, [edi]
    cmp dl, 0
    jne FORWARD_ONLY_RET        ; If square not empty, go back and give feedback
    
    F_ONLY_CONTINUE:
    mov cl, ch                  ; CheckPath uses cl as the loop condition, so put the forward jump distance into it

    ; normalize vectors
    call NormalizeVectors
    call CheckPath

    SKIP:
    ret
InputToMove ENDP

; Checks the path between source and destination to see if it's valid
; Input: esi starting position, edi ending position, ah y jump direction, al x jump direction, cl number of jumps, bh max number of jumps for piece type
CheckPath PROC uses esi
    ; Since we did x1-x2 and y1-y2 before, we have to invert the signs
    ; But since visually rows are in inverse position than they are in the array,
    ; we don't flip the y jump direction (Look at MoveSPointerToSquare for more info)
    neg al

    sal ah, 3           ; multiply row by 2^3 = 8
    cmp cl, bh
    jg MOVE_IS_INVALID  ; If # of jumps is greater than max, give feedback

    jmp LOOP_START      ; else skip feedback and continue

    MOVE_IS_INVALID:
    SetFeedback INVALID_MOVE
    jmp SKIP

    LOOP_START:
    ; Searching through path
    movsx edx, ah
    add esi, edx                ; Move over row
    movsx edx, al
    add esi, edx                ; Move over column

    mov dl, [esi]               ; Get next square in path

    dec cl
    jz AFTER_LOOP               ; If we have no more jumps, go to end of loop (We don't include the captured piece)

    cmp dl, 0
    jz LOOP_START               ; If square empty, continue loop

    SetFeedback BLOCKED_PATH    ; else set feedback and go back
    jmp SKIP

    AFTER_LOOP:

    cmp esi, edi    
    je KING_CHECK               ; If source and destination are equal, continue

    mov dl, [edi]               ; Else check en passant
    test dl, IS_EN_PASSANTABLE
    jnz SKIP                    ; If en passantable, skip king check (king will never be en passantable)

    SetFeedback ERROR_PATH      ; else set feedback
    jmp SKIP

    ; Check if king is trying to be captured
    KING_CHECK:
    mov al, [edi]                       ; Get square info
    and al, 0Fh                         ; Isolate piece type
    cmp al, KING
    jne SKIP                            ; If not trying to capture king, continue

    SetFeedback ERROR_KING_CAPTURE      ; Else give feedback

    SKIP:
    ret
CheckPath ENDP

; Assumes move is valid. Checks if path has any attacking pieces
; Input: dx starting position, ah y jump direction, al x jump direction, cl number of jumps
; Output: FEEDBACK is either 1 (has defenders) or 0 (has no defenders)
CheckPathForDefenders PROC uses esi
    SEARCH_START:
    call CheckForSquareDefender
    push ax
    GetFeedback
    cmp al, 1
    pop ax
    je SKIP

    add dl, al                  ; Move over row
    add dh, ah                  ; Move over column

    dec cl
    jnz SEARCH_START            ; If we still have jumps, continue loop

    SKIP:
    ret
CheckPathForDefenders ENDP

; Confirms that the move is valid by testing it on a copy of the chessboard
; If it is, do the move. if it isn't, produce feedback
; Input: esi starting square, edi ending square, 
VerifyMove PROC
    ; Make a copy of the chessboard
    call CopyChessboard
  
    ; Mark the piece as en passant if it's a pawn and a double square jump
    mov al, [esi]
    and al, 0Fh
    cmp al, PAWN
    jne CHECK_MOVE_TYPE     ; If not a pawn, continue to CHECK_MOVE_TYPE
    mov ebx, esi
    sub ebx, edi
    cmp ebx, 16
    je SET_EN_PASSANT
    neg ebx
    cmp ebx, 16
    jne CHECK_MOVE_TYPE
    
    SET_EN_PASSANT:
    mov al, [esi]
    or al, IS_EN_PASSANTABLE
    mov BYTE PTR [esi], al

    CHECK_MOVE_TYPE:
    mov al, [edi]      
    test al, IS_EN_PASSANTABLE
    jz NORMAL_MOVE              ; If destination square is not en passantable, go to NORMAL_MOVE

    mov al, [esi]               
    or al, HAS_MOVED            ; Mark HAS_MOVED bit for moving piece
    mov BYTE PTR [esi], 0
    mov BYTE PTR [edi], 0
    movsx ecx, ah               
    add edi, ecx                ; Move destination pointer forward one row
    mov BYTE PTR [edi], al
    jmp START_KING_SEARCH

    NORMAL_MOVE:
    mov al, [esi]
    or al, HAS_MOVED            ; Mark HAS_MOVED bit for moving piece
    mov BYTE PTR [edi], al      ; Overwrite destination with moving piece
    mov BYTE PTR [esi], 0       ; Cover up tracks

    START_KING_SEARCH:
    mov esi, OFFSET CHESSBOARD
    mov ah, GAME_STATUS
    and ah, IS_BLACK_TURN
    
    call MoveDXToKing
    

    push [edi]
    push edi
    call CheckForSquareAttacker
    pop edi
    pop [edi]
    GetFeedback
    cmp al, 0
    je SKIP
    
    ; UNDO CHANGE
    call UndoCopyChessboard
    SetFeedback KING_LEFT_IN_CHECK

    SKIP:
    ret
VerifyMove ENDP

; Checks if given square is being defended by current player
; Input: dx (Square coords), edi (Coords of piece that has just moved)
; Output: FEEDBACK is either 1 (true) or 0 (false)
CheckForSquareDefender PROC uses edx eax
    mov [userInput+2], dl 
    mov [userInput+3], dh 

    mov dl, 7
    mov dh, 7
    
    CHECK_FOR_ATTACKER:
    mov [userInput], dl
    mov [userInput+1], dh
    call InputToMove
    GetFeedback
    cmp al, ERROR_KING_CAPTURE
    je IS_ATTACKER
    cmp al, 0
    je IS_ATTACKER

    dec dl
    jns CHECK_FOR_ATTACKER
    mov dl, 7
    dec dh
    jns CHECK_FOR_ATTACKER
    SetFeedback 0
    jmp SKIP

    IS_ATTACKER:
    SetFeedback 1
    SKIP:
    ret
CheckForSquareDefender ENDP

; Checks if given square is being attacked by other player
; Input: dx (Square coords), edi (Coords of piece that has just moved)
; Output: FEEDBACK is either 1 (true) or 0 (false)
CheckForSquareAttacker PROC uses ax
    ; Temporarily switch turn to the opposite player (InputToMove needs this because we want to move the opposing player's pieces)
    mov al, GAME_STATUS
    xor al, IS_BLACK
    mov GAME_STATUS, al
    call CheckForSquareDefender
    mov al, GAME_STATUS
    xor al, IS_BLACK
    mov GAME_STATUS, al
    ret
CheckForSquareAttacker ENDP

; Input: dx (Square coords), edi (Coords of piece that has just moved)
; Output: cl (numbers of attackers), ch (number of pieces current player has), bx (Coords of attacker if any)
; Usage: cl will tell you how many times to pop stack
CheckForSquareAttackers PROC uses dx
    mov [userInput+2], dl 
    mov [userInput+3], dh 
    ; Temporarily switch turn to the opposite player (InputToMove needs this because we want to move the opposing player's pieces)
    mov al, GAME_STATUS
    xor al, IS_BLACK
    mov GAME_STATUS, al

    mov dl, 7
    mov dh, 7
    mov cx, 0
    mov bx, 0
    
    CHECK_FOR_ATTACKER:
    mov [userInput], dl
    mov [userInput+1], dh
    call InputToMove
    GetFeedback
    cmp al, ERROR_KING_CAPTURE
    je IS_ATTACKER
    cmp al, 0
    je IS_ATTACKER

    jmp NOT_ATTACKER

    IS_ATTACKER:
    mov bx, dx
    inc cl

    NOT_ATTACKER:    
    cmp al, MISMATCH_OWNER
    jne NOT_DEFENDER

    inc ch

    NOT_DEFENDER:
    dec dl
    jns CHECK_FOR_ATTACKER
    mov dl, 7
    dec dh
    jns CHECK_FOR_ATTACKER
    SetFeedback 0

    mov al, GAME_STATUS
    xor al, IS_BLACK
    mov GAME_STATUS, al
    ret
CheckForSquareAttackers ENDP

; Confirms whether current player's king has a valid move
; Input: dx (King)
; Output: FEEDBACK zero(true), non-zero(false)
CanKingMove PROC uses edx
    mov [userInput], dl 
    mov [userInput+1], dh 

    dec dl
    dec dh
    mov cl, 2
    mov ch, 2

    CHECK_MOVE:
    mov [userInput+2], dl
    mov [userInput+3], dh
    call InputToMove
    GetFeedback
    cmp al, 0
    je CHECK_VALID
    inc dl
    dec cl
    jns CHECK_MOVE
    mov cl, 2
    sub dl, 3
    inc dh
    dec ch
    jns CHECK_MOVE
    
    jmp SKIP

    CHECK_VALID:
    call VerifyMove
    GetFeedback
    cmp al, 0
    jne CHECK_MOVE
    
    call UndoCopyChessboard

    SKIP:
    ret
CanKingMove ENDP

; Simply gets the input from user and puts it in userInput
GetChessInput PROC
    mGotoxy 17,20               ; Move cursor after label "Enter your move: "

    mov edx, OFFSET userInput   ; ReadString takes edx as address
    mov ecx, INPUT_SIZE         ; ReadString takes ecx as the input size
    call ReadString

    ret
GetChessInput ENDP

; Gets input from userInput and converts the ascii characters to chessboard coords
ProcessInput PROC
    ; Convert characters to decimal values
    ; Letter
    mov al, [userInput]     ; First char
    sub al, 97              
    mov [userInput], al
    ; Number
    mov al, [userInput+1]   ; Second char
    sub al, 49
    mov [userInput+1], al
    ; Letter
    mov al, [userInput+2]   ; Third char
    sub al, 97
    mov [userInput+2], al
    ; Number
    mov al, [userInput+3]   ; Fourth char
    sub al, 49
    mov [userInput+3], al

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

    ; mov [CHESSBOARD+53], 0
    ; mov [CHESSBOARD+39], BLACK_QUEEN

    ret
InitChessboard ENDP

; Simply prints a label saying whose turn it is
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

; Prints the chessboard
PrintChessboard PROC

    mov cl, 8                           ; cl: columns
    mov ch, 8                           ; ch: rows
    mov dl, 1                           ; dl: incrementer for row/column labels
    mov ebx, 1                          ; ebx: incrementer for esi

    mov al, GAME_STATUS                 ; Get whose turn
    test al, IS_BLACK_TURN
    jnz PrintBlackTurn                  ; If black's turn, go to PrintBlackTurn

    PrintWhiteTurn:
        mov esi, OFFSET CHESSBOARD      ; Move source pointer to start of chessboard
        mov dh, 8                       ; Row labels start at 8...
        neg dl                          ; and count down
        jmp RowStart

    PrintBlackTurn:
        mov esi, OFFSET CHESSBOARD+63   ; Move source pointer to end of chessboard
        mov dh, 1                       ; Row labels start at 1 and count up
        neg ebx                         ; but esi counts down

    ; print the row number
    RowStart:
        mov eax, white + (black*16)     ; Set Text color to white
        call SetTextColor
        movzx eax, dh               
        call WriteDec                   ; Print Row Label
        mWrite <"   ">

    PieceStart:
        call PrintPiece         ; Print piece

        add esi, ebx            ; increment ebx
        dec cl                  ; decrement column counter
        jnz PieceStart          ; If cl != 0, continue printing pieces
        call crlf
        call crlf
        mov cl, 8               ; reset column counter
        add dh, dl              ; increment row label
        dec ch                  ; decrement row counter
        jnz RowStart            ; if ch != 0, continue printing rows

    ; Set Text color to white
    mov eax, white + (black*16)
    call SetTextColor

    ; Print the column labels
    neg dl                      ; Flip label incrementer...
    add dh, dl                  ; and undo the last add (this is important)
    mov cl, 8                   ; reset column counter

    mov al, dh                  ; White's view: dh = 1. Black's view: dh = 8 (without the undo, it would be 0 and 9)
    add al, 96                  ; 'a' - 1

    mWrite <"x   ">             ; Print corner

    ColumnLabel:
        call WriteChar          ; Print column label
        mWrite <"   ">
        add al, dl              ; increment column label
        dec cl                  ; decrement column counter
        jnz ColumnLabel         ; if c != 0, continue printing labels

    call crlf
    call crlf

    ret
PrintChessboard ENDP

; Gets character from esi and prints it
PrintPiece PROC uses edx    ; edx is used in PrintChessboard for nonvolitile temp information

    mov al, BYTE PTR [esi]  ; Get piece
    and al, 0Fh             ; Isolate type

    ; Check which piece type is on the square
    cmp al, PAWN
    je PrintPawn            ; If Pawn

    cmp al, ROOK
    je PrintRook            ; If Rook

    cmp al, KNIGHT
    je PrintKnight          ; If Knight

    cmp al, BISHOP
    je PrintBishop          ; If Bishop

    cmp al, QUEEN
    je PrintQueen           ; If Queen

    cmp al, KING
    je PrintKing            ; If King

    jmp PrintBlank          ; Else

    ; Save character to DL
    PrintPawn:
        mov dl, SYM_PAWN
        jmp CheckColor
    PrintRook:
        mov dl, SYM_ROOK
        jmp CheckColor
    PrintKnight:
        mov dl, SYM_KNIGHT
        jmp CheckColor
    PrintBishop:
        mov dl, SYM_BISHOP
        jmp CheckColor
    PrintQueen:
        mov dl, SYM_QUEEN
        jmp CheckColor
    PrintKing:
        mov dl, SYM_KING
        jmp CheckColor
    PrintBlank:
        mov dl, SYM_BLANK
        jmp ColorDefault    ; Skip the color check
        
    ; Check the color of square
    CheckColor:
        mov al, BYTE PTR [esi]      ; Get piece
        and al, IS_BLACK            ; Isolate color
        cmp al, IS_BLACK
        je ColorChangeToBlack
        jne ColorChangeToWhite

    ; Set Color
    ColorChangeToWhite:
    mov eax, yellow + (black*16)    ; White is represented with yellow
    jmp SetPrintColor
    ColorChangeToBlack:
    mov eax, red + (black*16)       ; Black is represented with red
    jmp SetPrintColor
    ColorDefault:
    mov eax, white + (black*16)     ; Blank squares are represented with white

    SetPrintColor:
    call SetTextColor

    ; Set stored character to al
    mov al, dl
    call WriteChar
    mWrite <"   ">

    ret
PrintPiece ENDP

PrintWinner PROC
    test al, IS_BLACK_TURN
    jnz BLACK_WON

    mWriteln "White Won!"
    jmp SKIP

    BLACK_WON:
    mWriteln "Black Won!"

    SKIP:
    ret
PrintWinner ENDP

CopyChessboard PROC uses esi edi cx
    mov esi, OFFSET CHESSBOARD
    mov edi, OFFSET CHESSBOARD_COPY

    mov cl, 64
    COPY:

        mov ch, [esi]
        mov BYTE PTR [edi], ch

        inc esi
        inc edi
        dec cl

    jnz COPY
    ret
CopyChessboard ENDP

UndoCopyChessboard PROC
    mov esi, OFFSET CHESSBOARD
    mov edi, OFFSET CHESSBOARD_COPY

    mov cl, 64
    COPY:

        mov ch, [edi]
        mov BYTE PTR [esi], ch

        inc esi
        inc edi
        dec cl

    jnz COPY
    ret
UndoCopyChessboard ENDP

; Input: esi (square to copy data from)
; Output: bl (Move Abilities), bh max # of jumps, FEEDBACK (EMPTY_SQUARE_MOVE)
GetPieceLogic PROC
    mov al, [esi]           ; Get square info
    and al, 0Fh             ; Isolate piece type
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

    ; If piece not identified, assume square is empty
    SetFeedback EMPTY_SQUARE_MOVE
    jmp AFTER_COPY_LOGIC
    
    ; This copies the movement logic of the piece type to bx.
    PAWN_LOGIC:
    mov bl, PAWN_MOVE       ; Copy PAWN_MOVE
    and bl, 0Fh             ; Isolate move abilities
    mov bh, PAWN_MOVE       ; Copy PAWN_MOVE
    shr bh, 4               ; Isolate max # of jumps
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
    ret
GetPieceLogic ENDP

END main