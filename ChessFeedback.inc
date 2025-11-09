COMMENT !

List of Macros:

SetFeedback MACRO type:REQ
	-- Sets the FEEDBACK variable to the given feedback type

GetFeedback MACRO
	-- Sets al to the value of FEEDBACK

PrintFeedback MACRO
	-- Prints the given feedback type in the FEEDBACK variable then clears the variable, else prints nothing

END COMMENT !

; Feedback Codes
INVALID_MOVE        EQU 1
MOVE_OUTSIDE_RANGE  EQU 2
KING_LEFT_IN_CHECK  EQU 3
EMPTY_SQUARE_MOVE   EQU 4
MISMATCH_OWNER      EQU 5
MISMATCH_COLOR_CAPT EQU 6
BLOCKED_PATH        EQU 7
ERROR_PATH          EQU 8
ERROR_KING_CAPTURE  EQU 9
SAME_SQUARE         EQU 10

.data
	FEEDBACK BYTE 0

	INVALID_MOVE_STR		BYTE "Invalid Move!",13,10,0
	MOVE_OUTSIDE_RANGE_STR	BYTE "Invalid Syntax / Outside Range!",13,10,0
	KING_LEFT_IN_CHECK_STR	BYTE "Your king is left in check!",13,10,0
	EMPTY_SQUARE_MOVE_STR	BYTE "Starting square is empty!",13,10,0
	MISMATCH_OWNER_STR		BYTE "Can't move piece that's not yours!",13,10,0
	MISMATCH_COLOR_CAPT_STR	BYTE "You already occupying the end square!",13,10,0
	BLOCKED_PATH_STR		BYTE "Path between start and end is blocked by a piece!",13,10,0
	ERROR_PATH_STR			BYTE "A bug occurred when checking the path between pieces",13,10,0
	ERROR_KING_CAPTURE_STR	BYTE "You should never be able to capture the king. That's a bug",13,10,0
    SAME_SQUARE_STR         BYTE "Starting square and ending square can't be the same!",13,10,0

SetFeedback MACRO type:REQ
	mov FEEDBACK, type
ENDM

GetFeedback MACRO
	mov al, FEEDBACK
ENDM

PrintFeedback MACRO

    ; Set text color to red
    mov eax, red + (black*16)
    call SetTextColor

    mov al, FEEDBACK
    cmp al, 0
    je SKIP_PRINT               ; skip to end if there is no feedback

    ; Check if FEEDBACK corresponds to a feedback code
    cmp al, INVALID_MOVE
    je INVALID_MOVE_F

    cmp al, MOVE_OUTSIDE_RANGE
    je MOVE_OUTSIDE_RANGE_F

    cmp al, MISMATCH_OWNER
    je MISMATCH_OWNER_F

    cmp al, MISMATCH_COLOR_CAPT
    je MISMATCH_COLOR_CAPT_F

    cmp al, BLOCKED_PATH
    je BLOCKED_PATH_F

    cmp al, KING_LEFT_IN_CHECK
    je KING_LEFT_IN_CHECK_F

    cmp al, EMPTY_SQUARE_MOVE
    je EMPTY_SQUARE_MOVE_F

    cmp al, ERROR_PATH
    je ERROR_PATH_F

    cmp al, ERROR_KING_CAPTURE
    je ERROR_KING_CAPTURE_F

    cmp al, SAME_SQUARE
    je SAME_SQUARE_F
    
    jmp SKIP_PRINT              ; If no feedback was found, skip to the end

    ; Set edx to the corresponding feedback string
    INVALID_MOVE_F:
        mov edx, OFFSET INVALID_MOVE_STR
        jmp END_OF_FEEDBACK

    MISMATCH_OWNER_F:
        mov edx, OFFSET MISMATCH_OWNER_STR
        jmp END_OF_FEEDBACK

    MISMATCH_COLOR_CAPT_F:
        mov edx, OFFSET MISMATCH_COLOR_CAPT_STR
        jmp END_OF_FEEDBACK

    MOVE_OUTSIDE_RANGE_F:
        mov edx, OFFSET MOVE_OUTSIDE_RANGE_STR
        jmp END_OF_FEEDBACK

    KING_LEFT_IN_CHECK_F:
        mov edx, OFFSET KING_LEFT_IN_CHECK_STR
        jmp END_OF_FEEDBACK

    EMPTY_SQUARE_MOVE_F:
        mov edx, OFFSET EMPTY_SQUARE_MOVE_STR
        jmp END_OF_FEEDBACK

    BLOCKED_PATH_F:
        mov edx, OFFSET BLOCKED_PATH_STR
        jmp END_OF_FEEDBACK

    ERROR_KING_CAPTURE_F:
        mov edx, OFFSET ERROR_KING_CAPTURE_STR
        jmp END_OF_FEEDBACK

    ERROR_PATH_F:
        mov edx, OFFSET ERROR_PATH_STR
        jmp END_OF_FEEDBACK

    SAME_SQUARE_F:
        mov edx, OFFSET SAME_SQUARE_STR


    END_OF_FEEDBACK:
    call WriteString                ; Print out feedback in edx

    SKIP_PRINT:
    mov eax, white + (black*16)     ; Set text color back to white
    call SetTextColor

ENDM