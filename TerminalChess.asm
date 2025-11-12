; Author: Matthew Marquez
; Terminal Chess written in assembly

include Irvine32.inc
include Macros.inc

include SmallHelpers.inc
include ChessFeedback.inc
include ChessHistory.inc

INPUT_SIZE EQU 5

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

; Piece Data
IS_BLACK            EQU 10000000b   ; WHITE 0 / BLACK 1
HAS_MOVED           EQU 01000000b   ; The piece has moved
WAS_EN_PASSANTED    EQU 00100000b   ; This is used in ChessHistory.asm to save if move was en passant
IS_EN_PASSANTABLE   EQU 00010000b   ; The piece is en passantable
NO_EN_PASSANT       EQU 11101111b   ; Clear en passant mask

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
CHECKMATE       EQU 00000100b
IS_BLACK_TURN   EQU 10000000b


.data

    resign_str BYTE "resi",0
    exit_str BYTE "exit",0
    chess_title BYTE "Terminal Chess",0
    userInput BYTE INPUT_SIZE DUP(0)

    ; 4 left bits hold max number of jumps, 4 right bits hold move abilities (diagonal, parallel, etc)
    KING_MOVE BYTE      00010011b   ; 1 max jump    diagonal, parallel
    QUEEN_MOVE BYTE     10000011b   ; 8 max jumps   diagonal, parallel
    KNIGHT_MOVE BYTE    00010100b   ; 1 max jump    L shape
    BISHOP_MOVE BYTE    10000001b   ; 8 max jumps   diagonal
    ROOK_MOVE BYTE      10000010b   ; 8 max jumps   parallel
    PAWN_MOVE BYTE      00101000b   ; 2 max jumps   forward only

    GAME_STATUS BYTE 0
    FIFTY_MOVE_RULE BYTE 0

    CHESSBOARD          BYTE 64 DUP (0)
    CHESSBOARD_COPY     BYTE 64 DUP (0)

.code

main PROC
    invoke SetConsoleTitle, OFFSET chess_title
    PLAY_AGAIN:
    call chess
    je QUIT_GAME

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
    ; Clear Feedback
        SetFeedback 0
    ; Clear registers
        mov eax, 0
        mov ebx, 0
        mov ecx, 0
        mov edx, 0
        mov esi, 0
        mov edi, OFFSET CHESSBOARD_COPY     ; Just so the first push at GetInput doesn't give an error
    ; Reset data
    mov [GAME_STATUS], 0
    mov [FIFTY_MOVE_RULE], 0
    call ResetChessHistory

    jmp StartTurn
        
    StartTurnError:
    ; If invalid move, we want to load last moved piece
        pop edi                 ; address
        pop ax                  ; then value
        mov [edi], al
    StartTurn:
        call Clrscr             ; Clear Screen
        call PrintChessboard    ; Print Chessboard
        
        call PrintWhoseTurn     ; Print the "Enter your move" label
        PrintFeedback           ; Print feedback if any
    
    GetInput:
        mov al, [edi]           ; Save last moved piece (This is the last light blue piece)
        push ax                 ; value
        push edi                ; then address
        ; Clear Feedback
        SetFeedback 0
        ; Clear registers
        mov eax, 0
        mov ebx, 0
        mov ecx, 0
        mov edx, 0
        mov esi, 0
        mov edi, 0
        call GetChessInput      ; Ask the player for input

        Invoke Str_compare, OFFSET userInput, OFFSET exit_str 
        je EXIT_L
        Invoke Str_compare, OFFSET userInput, OFFSET resign_str
        je IS_RESIGN

        call ProcessInput           ; Turns ascii input in userInput into coords ranging from 0-7
        GetFeedback
        cmp al, 0
        jne StartTurnError          ; If FEEDBACK != 0, restart turn

    InputValidation:
        ; Pre-move Validation
        call PreMoveValidation      ; Lots of small checks for pre move validation
        GetFeedback
        cmp al, 0
        jne StartTurnError          ; If FEEDBACK != 0, restart turn

        ; In-move Validation
        call InMoveValidation       ; Check if pathing is valid
        GetFeedback
        cmp al, 0
        jne StartTurnError          ; If FEEDBACK != 0, restart turn

        call SetCapturedPiece       ; Set ChessHistory captured move
        call SetMovedPieceBefore    ; Set ChessHistory moved piece before

        ; Post-move Validation
        call PostMoveValidation     ; On a copy, do the move and test if king is in check
        GetFeedback                 ; move FEEDBACK Byte into al
        cmp al, 0
        jne StartTurnError          ; If FEEDBACK != 0, restart turn

    ; Input is valid
    call PawnPromotion              ; Check if Pawn Promotion is in play

    ; Save History
    call SetMovedPieceAfter
    call SetMoveCoords
    call IncrementChessHistory

    ; Switch turn
    mov al, GAME_STATUS
    xor al, IS_BLACK_TURN
    mov GAME_STATUS, al

    ; Increment 50 move rule
    mov al, FIFTY_MOVE_RULE
    inc al
    mov FIFTY_MOVE_RULE, al

    add esp, 6                  ; We clear last moved piece from stack since current move is valid

    mov al, [edi]
    call CheckmateCondition
    mov [edi], al

    mov al, GAME_STATUS
    test al, GAMEOVER
    jnz GAME_IS_OVER

    call StalemateCondition

    GetFeedback
    cmp al, 0
    jne GAME_IS_OVER

    jmp StartTurn               ; Loop back
    

    IS_RESIGN:
    ; Gameover
        add esp, 6              ; Clear last moved piece from stack
        mov al, GAME_STATUS
        or al, GAMEOVER
        or al, CHECKMATE
        mov GAME_STATUS, al
    GAME_IS_OVER:
    call Clrscr                 ; Clear Screen
    call PrintChessboard        ; Print Chessboard

    mov al, GAME_STATUS

    test al, CHECKMATE
    jnz IS_CHECKMATE

    test al, STALEMATE
    jnz IS_STALEMATE

    jmp UNKNOWN_ERROR

    IS_CHECKMATE:
        call PrintWinner
    ret

    IS_STALEMATE:
        call PrintStalemate
    ret
    UNKNOWN_ERROR:
        mWriteln "Game ended wrongly. This is a bug."
    ret
    EXIT_L:
        add esp, 6              ; Clear last moved piece from stack
        test al, 0              ; Set Zero flag
    ret
chess ENDP

; A series of checks to determine if player has been checkmated
; Input: 
; Output: 
CheckmateCondition PROC uses edi ax
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
    jg IS_CHECKMATE             ; If king can't move and checked by > one piece, it is checkmate
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
    je IS_CHECKMATE

    jmp SKIP

    IS_CHECKMATE:
        mov al, GAME_STATUS
        or al, GAMEOVER
        or al, CHECKMATE
        mov GAME_STATUS, al
    SKIP:
        SetFeedback 0
    ret
CheckmateCondition ENDP

; Checks if game is in Stalemate
; Output: Feedback (0 if not stalemate, else is stalemate)
StalemateCondition PROC uses edi
    ; Set coords and pointer to start of array
    mov dl, 0
    mov dh, 7
    mov esi, OFFSET CHESSBOARD

    ; Prepare ah with whose turn it is
    mov ah, GAME_STATUS         ; Get Game Status
    and ah, IS_BLACK_TURN       ; Isolate game status's is_black_turn bit

    ; 1) Check Chess History for Threefold Repetition
    call CheckThreefoldRepetition
    jz IS_STALEMATE                    ; If zero flag set, is stalemate

    ; 2) Search through array, simulate all moves for pieces current player owns
    SEARCH_FOR_P:
        mov al, [esi]               ; Get square info
        cmp al, 0
        je SKIP_PIECE               ; If square == 0 (empty), skip this piece
        and al, IS_BLACK            ; Isolate piece's is_black bit
        xor al, ah                  ; Check if player owns piece
        jnz SKIP_PIECE              ; If different color, skip this piece

        CHECK_MOVES:
        call SimulateAllMoves
        GetFeedback
        cmp al, 0
        je FIFTY_RULE               ; If valid move, check more conditions

        SKIP_PIECE:
        inc esi
        inc dl                      ; Look at next column
        cmp dl, 8
        jl SEARCH_FOR_P             ; If column < 8, go back
        mov dl, 0                   ; reset column to 0
        dec dh                      ; Look at next row
        jns SEARCH_FOR_P            ; If row >= 0, go back

    jmp IS_STALEMATE            ; If found no valid moves, is stalemate
    
    ; 3) 50 move rule (UNFINISHED)
    FIFTY_RULE:
        ;mov al, FIFTY_MOVE_RULE
        ;cmp FIFTY_MOVE_RULE, 99
        ;jg IS_STALEMATE

    ; 4) insufficient material (UNIMPLEMENTED)

    jmp SKIP

    IS_STALEMATE:
        SetFeedback 1
        mov al, GAME_STATUS
        or al, GAMEOVER
        or al, STALEMATE
        mov GAME_STATUS, al

    SKIP:
    ret
StalemateCondition ENDP

; Input: esi (moving piece), dx (piece coordinates)
; Output: Feedback (0 for valid move, else for invalid)
SimulateAllMoves PROC uses edi ax
    call GetPieceLogic
    ; Check all of the piece's move abilities to check for valid moves
    test bl, DIAGONAL
    jz NOT_DIAGONAL
        mov ch, 4
        mov eax, 0FFFF0101h ; x1y1 then x1y-1 then x-1y-1 then x-1y1
        call SimulateMoves
        GetFeedback
        cmp al, 0
        je SKIP
    NOT_DIAGONAL:

    test bl, PARALLEL
    jz NOT_PARALLEL
        mov ch, 4
        mov eax, 00FF0001h  ; x1y0 then x0y-1 then x-1y0 then x0y1
        call SimulateMoves
        GetFeedback
        cmp al, 0
        je SKIP
    NOT_PARALLEL:

    test bl, LSHAPE
    jz NOT_LSHAPE           ; (I drew a diagram to brainstorm this one)
        ; First 4 moves 
        mov ch, 4
        mov eax, 0FE010201h ; x1y2 then x2y1 then x1y-2 then x-2y1
        call SimulateMoves
        GetFeedback
        cmp al, 0
        je SKIP
        ; Last 4 moves
        mov ch, 4
        mov eax, 0FEFF02FFh ; x-1y2 then x2y-1 then x-1y-2 then x-2y-1
        call SimulateMoves
        GetFeedback
        cmp al, 0
        je SKIP
    NOT_LSHAPE:

    test bl, FORWARD_ONLY
    jz NOT_FORWARD_ONLY
        ; First 2 moves
        mov ch, 2
        mov eax, 010100h    ; x0y1 then x1y1
        call SimulateMoves
        GetFeedback
        cmp al, 0
        je SKIP
        ; Last move
        mov ch, 1
        mov eax, 01FFh      ; x-1y1
        call SimulateMoves
        GetFeedback
        cmp al, 0
        je SKIP
    NOT_FORWARD_ONLY:

    SetFeedback 1           ; If none were valid, set feedback

    SKIP:
    ret
SimulateAllMoves ENDP

; Simulates 4 moves by looking at bx then rotating ebx by a byte
; Input: ebx (The moves to do), ch (How many moves to do. Required 1-4), 
;        esi (Moving piece), dx (coords of moving piece)
; Output: Feedback (0 if valid move, else if invalid)
SimulateMoves PROC uses esi eax ebx ecx edx

    SIM_START:
        mov bx, dx                  ; Make copy of source square
        ror edx, 16                 ; stash it
        mov dx, bx                  ; paste copy in destination
        add dx, ax                  ; Add move coords to destination
        test dx, 0F8F8h             ; Check if dl and dh are between 0 and 7 inclusive

        ror edx, 16                 ; Switch back to source
        jnz SKIP_SIM                ; If not in range, skip

        call PreMoveValidation
        mov bl, al
        GetFeedback
        cmp al, 0
        mov al, bl
        jne SKIP_SIM

        call InMoveValidation
        mov bl, al
        GetFeedback
        cmp al, 0
        mov al, bl
        jne SKIP_SIM                ; If there is feedback (!= 0), invalid move

        call PostMoveValidation
        mov bl, al
        GetFeedback
        cmp al, 0
        mov al, bl
        jne SKIP_SIM                ; If there is feedback (!= 0), invalid move

    ; Valid Move
    call UndoCopyChessboard
    jmp SKIP

    SKIP_SIM:
        ror eax, 8
        dec ch
    jnz SIM_START

    SetFeedback 1           ; If none were valid, set feedback

    SKIP:        
    ret
SimulateMoves ENDP

; A series of premove validations
; Input: edx (Position in this order: y2x2y1x1)
; Output: Feedback (0 if valid move, else if invalid), esi (source square), edi (destination square)
PreMoveValidation PROC uses edx ecx ebx eax
    SetFeedback 0

    ; Check if source and destination coords are the same
    mov bx, dx                      ; Copy source coords to bx
    ror edx, 16                     ; Temp switch dx to the destination coords
    cmp bx, dx
    ror edx, 16
    jne NOT_SAME_SQUARE             ; if source != destination, continue

    SetFeedback SAME_SQUARE         ; else set feedback and skip to the end
    jmp SKIP

    NOT_SAME_SQUARE:
    ; Getting the source square
    mov esi, OFFSET CHESSBOARD
    call MoveSPointerToSquare       ; Move esi to coords in dx

    ; Remove IS_EN_PASSANTABLE (Pawns can only be en passanted the move after, so we're removing the attribute)
    mov bl, [esi]
    and bl, NO_EN_PASSANT
    mov BYTE PTR [esi], bl
    
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
    and al, IS_BLACK            ; Isolate piece's is_black bit
    and ah, IS_BLACK_TURN       ; Isolate game status's is_black_turn bit
    xor al, ah                  ; Check if the color is the same
    jz SOURCE_CORRECT_COLOR     ; If same, continue

    SetFeedback MISMATCH_OWNER
    jmp SKIP

    SOURCE_CORRECT_COLOR:
    ; Getting the destination square
    ror edx, 16                 ; Switch dx to the destination square
    mov edi, OFFSET CHESSBOARD
    call MoveDPointerToSquare   ; Move edi to coords in dx
    ror edx, 16                 ; Switch dx back to source square
    ; Check if there is a piece there
    mov al, [edi]               ; Get destination square info
    cmp al, 0
    jne CHECK_COLOR             ; If al != 0 (not empty), check color

    jmp DONT_CHECK_COLOR        ; else don't check color

    CHECK_COLOR:
    ; Check if captured piece is the player's
    ; (Note: ah already has game status's color bit from earlier)
    mov al, [edi]                   ; Get destination square info
    and al, IS_BLACK                ; Isolate is_black bit
    xor al, ah                      ; Check if the color is the same
    jnz DONT_CHECK_COLOR            ; If different, continue

    SetFeedback MISMATCH_COLOR_CAPT
    jmp SKIP

    DONT_CHECK_COLOR:
    ; Now we're going to do pathing validations
    

    SKIP:
    ret
PreMoveValidation ENDP

; Checks if the given coords match a move type (parallel, diagonal, etc),
; Then checks if path is valid
; Input: edx (Position in this order: y2x2y1x1), esi (source square), edi (destination square)
; Output: Feedback (0 if valid move, else if invalid), edi (moved to captured piece if en passant)
InMoveValidation PROC uses eax ebx ecx edx
    ; Check piece type
    call GetPieceLogic
    GetFeedback    
    cmp al, 0

    jne SKIP

    call GetCoordsDifference    ; Get difference in coords in edx. al(x) and ah(y) are normal, cl(x) and ch(y) are absolute value
    
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
    ; Check double jump
    cmp ch, 1
    jg CHECK_DOUBLE             ; If double jump (y > 1), check double jump

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
    and dl, IS_BLACK            ; Isolate piece's is_black bit
    and dh, IS_BLACK_TURN       ; Isolate game status's is_black_turn bit
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
    mov dh, ah                  ; move change in y into dx
    mov dl, GAME_STATUS
    and dl, IS_BLACK_TURN       ; Isolate game status's IS_BLACK_TURN bit
    xor dh, GAME_STATUS         ; bit is 1 when correct direction
    test dh, IS_BLACK_TURN
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
InMoveValidation ENDP

; Checks the path between source and destination to see if it's valid
; Input: esi starting position, edi ending position, ah y jump direction, al x jump direction, 
;        cl number of jumps, bh max number of jumps for piece type
CheckPath PROC uses esi eax cx edx
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

; Confirms that the move is valid by testing it on a copy of the chessboard
; Input: esi starting square, edi ending square, 
PostMoveValidation PROC uses eax ecx ebx edx
    ; Make a copy of the chessboard
    call CopyChessboard
  
    ; Mark the piece as en passant if it's a pawn and a double square jump
    ; Check if pawn
    mov al, [esi]           ; Get piece
    and al, 0Fh             ; Isolate type
    cmp al, PAWN
    jne CHECK_MOVE_TYPE     ; If not a pawn, continue to CHECK_MOVE_TYPE
    mov ebx, esi
    sub ebx, edi
    cmp ebx, 16             ; If move is exactly 2 rows (esi-edi == 16), set en passant
    je SET_EN_PASSANT
    neg ebx
    cmp ebx, 16             ; If move is exactly 2 rows (-(esi-edi) == 16), set en passant
    jne CHECK_MOVE_TYPE
    
    SET_EN_PASSANT:
    mov al, [esi]
    or al, IS_EN_PASSANTABLE
    mov BYTE PTR [esi], al

    CHECK_MOVE_TYPE:
    mov al, [edi]               ; Get destination square    
    test al, IS_EN_PASSANTABLE
    jz NORMAL_MOVE              ; If not en passantable, go to NORMAL_MOVE

    call GetCoordsDifference
    shl ah, 3                   ; Multiply y * 2^3
    movsx ecx, ah               ; Expand row
    movsx ebx, al               ; Expand column
    neg ebx

    mov al, [esi]               ; Copy source square  
    or al, HAS_MOVED            ; Mark HAS_MOVED bit for moving piece
    mov [esi], BYTE PTR 0       ; Clear out source and destination
    mov [edi], BYTE PTR 0
    
    mov edi, esi
    add edi, ecx                ; Move destination pointer forward one row
    add edi, ebx                ; Move destination pointer horizontally by one column
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
    
    mov al, [edi]
    call CheckForSquareAttacker
    mov [edi], al
    GetFeedback
    cmp al, 0
    je SKIP                     ; If no attacker, continue
    
    ; UNDO CHANGE
    call UndoCopyChessboard
    SetFeedback KING_LEFT_IN_CHECK

    SKIP:
    ret
PostMoveValidation ENDP

; Check if the previous move was a pawn,
; If so, check if it needs to be promoted, then promote
; Input: edi (piece that just moved)
; Output: edi (changes made)
PawnPromotion PROC uses ebx ecx edx
    mov al, [edi]           ; Get previous piece
    mov bl, al              ; Make a copy
    and bl, 0F0h            ; Clear piece type from copy
    and al, 00Fh             ; Isolate piece type
    cmp al, PAWN
    mov al, 0               ; zero out al
    jne SKIP                ; If not a pawn, skip

    ; else continue
    ror edx, 16             ; Switch coords to destination
    cmp dh, 7
    je DO_PROMOTION         ; If y == 7, do promotion
    cmp dh, 0
    jne SKIP                ; If y != 0, skip

    ; else, do promotion
    DO_PROMOTION:
    mWrite "Promote your pawn  (Q, R, N, B): "
    GET_INPUT:
    call ReadChar
    and al, 11011111b

    cmp al, SYM_QUEEN
    je QUEEN_P

    cmp al, SYM_ROOK
    je ROOK_P

    cmp al, SYM_KNIGHT
    je KNIGHT_P

    cmp al, SYM_BISHOP
    je BISHOP_P

    jmp GET_INPUT

    QUEEN_P:
        mov al, QUEEN           ; Set al to QUEEN value
        or bl, al               ; Change piece in ah to the new piece
        jmp END_PROMOTION
    ROOK_P:
        mov al, ROOK            ; Set al to ROOK value
        or bl, al               ; Change piece in ah to the new piece
        jmp END_PROMOTION
    KNIGHT_P:
        mov al, KNIGHT          ; Set al to KNIGHT value
        or bl, al               ; Change piece in ah to the new piece
        jmp END_PROMOTION
    BISHOP_P:
        mov al, BISHOP          ; Set al to BISHOP value
        or bl, al               ; Change piece in ah to the new piece

    END_PROMOTION:
    mov [edi], bl

    SKIP:
    ret
PawnPromotion ENDP

; Checks if given square is being defended by current player
; Input: dx (Square coords), edi (Coords of piece that has just moved)
; Output: FEEDBACK is either 1 (true) or 0 (false)
CheckForSquareDefender PROC uses edx eax edi
    ror edx, 16

    mov dl, 7
    mov dh, 7
    
    CHECK_FOR_DEFENEDER:
    SetFeedback 0
    call PreMoveValidation
    GetFeedback
    cmp al, 0
    jne NOT_DEFENDER

    call InMoveValidation
    GetFeedback
    cmp al, ERROR_KING_CAPTURE
    je IS_DEFENDER
    cmp al, 0
    je IS_DEFENDER

    NOT_DEFENDER:
    dec dl
    jns CHECK_FOR_DEFENEDER
    mov dl, 7
    dec dh
    jns CHECK_FOR_DEFENEDER
    SetFeedback 0
    jmp SKIP

    IS_DEFENDER:
    SetFeedback 1
    SKIP:
    ret
CheckForSquareDefender ENDP

; Assumes move is valid. Checks if attack path has any defending pieces
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
; Checks if given square is being attacked by other player
; Input: dx (Square coords), edi (Coords of piece that has just moved)
; Output: FEEDBACK is either 1 (true) or 0 (false)
CheckForSquareAttacker PROC uses ax
    ; Temporarily switch turn to the opposite player (We want to move the opposing player's pieces)
    mov al, GAME_STATUS
    xor al, IS_BLACK_TURN
    mov GAME_STATUS, al
    call CheckForSquareDefender
    mov al, GAME_STATUS
    xor al, IS_BLACK_TURN
    mov GAME_STATUS, al
    ret
CheckForSquareAttacker ENDP

; Input: dx (Square coords), edi (Coords of piece that has just moved)
; Output: cl (numbers of attackers), ch (number of pieces current player has), bx (Coords of attacker if any)
CheckForSquareAttackers PROC uses edx edi
    ror edx, 16
    ; Temporarily switch turn to the opposite player (InMoveValidation needs this because we want to move the opposing player's pieces)
    mov al, GAME_STATUS
    xor al, IS_BLACK_TURN
    mov GAME_STATUS, al

    mov dl, 7
    mov dh, 7
    mov cx, 0
    mov bx, 0
    
    CHECK_FOR_ATTACKER:
    SetFeedback 0
    call PreMoveValidation
    GetFeedback
    cmp al, MISMATCH_OWNER
    je IS_DEFENDER
    cmp al, 0
    jne SKIP_MOVE

    call InMoveValidation
    GetFeedback
    cmp al, ERROR_KING_CAPTURE
    je IS_ATTACKER
    cmp al, 0
    je IS_ATTACKER

    jmp SKIP_MOVE

    IS_ATTACKER:
    mov bx, dx
    inc cl

    jmp SKIP_MOVE

    IS_DEFENDER:
    inc ch
    SKIP_MOVE:
    dec dl
    jns CHECK_FOR_ATTACKER
    mov dl, 7
    dec dh
    jns CHECK_FOR_ATTACKER

    mov al, GAME_STATUS
    xor al, IS_BLACK_TURN
    mov GAME_STATUS, al
    ret
CheckForSquareAttackers ENDP

; Confirms whether current player's king has a valid move
; Input: dx (King)
; Output: FEEDBACK zero(true), non-zero(false)
CanKingMove PROC uses edx edi
    push dx
    ror edx, 16
    pop dx

    dec dl
    dec dh
    mov cl, 2
    mov ch, 2

    CHECK_MOVE:
    SetFeedback 0
    call PreMoveValidation
    GetFeedback
    cmp al, 0
    jne SKIP_MOVE

    call InMoveValidation
    GetFeedback
    cmp al, 0
    jne SKIP_MOVE

    call PostMoveValidation
    GetFeedback
    cmp al, 0
    jne SKIP_MOVE

    call UndoCopyChessboard
    jmp SKIP

    SKIP_MOVE:
    inc dl
    dec cl
    jns CHECK_MOVE
    mov cl, 2
    sub dl, 3
    inc dh
    dec ch
    jns CHECK_MOVE

    SKIP:
    ret
CanKingMove ENDP

; Simply gets the input from user and puts it in userInput
GetChessInput PROC
    mGotoxy 17,20               ; Move cursor after label "Enter your move: "

    mov edx, OFFSET userInput   ; ReadString takes edx as buffer address
    mov ecx, INPUT_SIZE         ; ReadString takes ecx as the input size
    call ReadString

    ret
GetChessInput ENDP

; Gets input from userInput and converts the ascii characters to chessboard coords
; Input: userInput
; Output: edx (Converted userInput)
ProcessInput PROC
    ; Convert characters to decimal values
    ; userInput (a2c4[0123] in memory -> 4c2a[3210] in edx)

    ; Letter
    mov al, [userInput+2]   ; Third char
    sub al, 97
    mov dl, al          ; Move into dl (x)
    ; Number
    mov al, [userInput+3]   ; Fourth char
    sub al, 49
    mov dh, al          ; Move into dh (y)

    ror edx, 16

    ; Letter
    mov al, [userInput]     ; First char
    sub al, 97              
    mov dl, al          ; Move into dl (x)
    ; Number
    mov al, [userInput+1]   ; Second char
    sub al, 49
    mov dh, al          ; Move into dh (y)

    NotSameSquare:
    ; Check if coords are inside range (0-7)
    test edx, 0F8F8F8F8h
    jz SKIP

    SetFeedback MOVE_OUTSIDE_RANGE
    SKIP:
    ret
ProcessInput ENDP

InitChessboard PROC
    ; Clear board
    mov cl, 64
    mov esi, OFFSET [CHESSBOARD]

    PlaceBlanks:
    mov BYTE PTR [esi], 0
    inc esi
    dec cl
    jnz PlaceBlanks

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
    mWrite "Current History Length: "
    call GetChessHistoryLength
    call WriteDec
    call Crlf

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
        cmp esi, edi
        je ColorChangeToRecentPiece
        mov al, BYTE PTR [esi]      ; Get piece
        and al, IS_BLACK            ; Isolate color
        cmp al, IS_BLACK
        je ColorChangeToBlack
        jne ColorChangeToWhite

    ; Set Color
    ColorChangeToWhite:
        mov eax, yellow + (black*16)        ; White is represented with yellow
        jmp SetPrintColor
    ColorChangeToBlack:
        mov eax, red + (black*16)           ; Black is represented with red
        jmp SetPrintColor
    ColorChangeToRecentPiece:
        mov eax, lightBlue + (black*16)     ; Recent piece is represented with light blue
        jmp SetPrintColor
    ColorDefault:
        mov eax, white + (black*16)         ; Blank squares are represented with white

    SetPrintColor:
    call SetTextColor

    ; Set stored character to al
    mov al, dl
    call WriteChar
    mWrite <"   ">

    ret
PrintPiece ENDP

; Prints the winner
; Input: al (GAME_STATUS)
PrintWinner PROC
    test al, IS_BLACK_TURN
    jz BLACK_WON                ; If it's currently white's turn, that means black won

    mWriteln "White Won!"
    jmp SKIP

    BLACK_WON:
    mWriteln "Black Won!"

    SKIP:
    ret
PrintWinner ENDP

; Prints Stalemate
PrintStalemate PROC
    ;test al, IS_BLACK_TURN
    ;jz WHITE_NO_MOVES           ; If it's currently white's turn, that means white has no moves

    mWriteln "NO ONE WINS LOSERSSSSS"

    ret
PrintStalemate ENDP

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

UndoCopyChessboard PROC uses esi edi
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
