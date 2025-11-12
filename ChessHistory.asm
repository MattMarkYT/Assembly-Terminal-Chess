INCLUDE Irvine32.inc
INCLUDE ChessHistory.inc



.data

	ChessMove struct
		moveCoords 			DWORD 0		; y2-x2-y1-x1
		capturedPiece 		BYTE 0		; Captured piece, if applicable
		movedPieceBefore 	BYTE 0		; Moved piece before move
		movedPieceAfter		BYTE 0		; Moved piece after move
		endingMove			BYTE 0		; This value decides whether history should end here
	ChessMove ENDS
	moveHistory ChessMove 2048 DUP (<?,?,?,?>) 	; Save up to 2048 moves
	moveHistoryLen DWORD 0						; The length of the array

.code

; Sets current ChessMove move coordinates
; Input: edx (move coordinates y2-x2-y1-x1)
SetMoveCoords PROC uses eax

	mov eax, moveHistoryLen			; Get length
	mov [moveHistory + eax].moveCoords, edx

	ret
SetMoveCoords ENDP

; Sets current ChessMove captured piece
; Input: edi (captured piece)
SetCapturedPiece PROC uses eax bx
	
	mov eax, moveHistoryLen			; Get length
	mov bl, [edi]					; Get captured piece
	mov [moveHistory + eax].capturedPiece, bl

	ret
SetCapturedPiece ENDP

; Sets current ChessMove captured piece
; Input: esi (moving piece)
SetMovedPieceBefore PROC uses eax bx

	mov eax, moveHistoryLen			; Get length
	mov bl, [edi]					; Get moved piece
	mov [moveHistory + eax].movedPieceBefore, bl

	ret
SetMovedPieceBefore ENDP

; Sets current ChessMove captured piece
; Input: edi (moved piece)
SetMovedPieceAfter PROC uses eax bx

	mov eax, moveHistoryLen			; Get length
	mov bl, [edi]					; Get moved piece
	mov [moveHistory + eax].movedPieceAfter, bl

	ret
SetMovedPieceAfter ENDP

; Increments length to point to the next ChessMove
IncrementChessHistory PROC uses eax

	mov eax, moveHistoryLen			; Get length
	add eax, SIZEOF ChessMove		; increment length by struct size
	mov	moveHistoryLen, eax
	
	ret
IncrementChessHistory ENDP

; Resets length to 0, clearing history
ResetChessHistory PROC

	mov	moveHistoryLen, 0
	ret
ResetChessHistory ENDP

; Get the current value of length
; Output: eax (length)
GetChessHistoryLength PROC

	mov eax, moveHistoryLen
	ret
GetChessHistoryLength ENDP

; Checks history to see if threefold repetition happened
; Output: Zero flag (1 if true, 0 if false)
CheckThreefoldRepetition PROC uses eax ebx cx esi edi

	mov eax, moveHistoryLen			; length
	mov ebx, SIZEOF ChessMove
	shl ebx, 3						; 2^3 = 8
	add ebx, SIZEOF ChessMove		; 8 + 1 = 9

	cmp eax, ebx					; cmp length, 9
	jl SKIP							; if (length < 9), not threefold repetition

	; Loop to check repetition
	mov esi, OFFSET moveHistory
	sub esi, SIZEOF ChessMove		; esi = moveArray.get(len-1)
	mov edi, esi

	mov ebx, SIZEOF ChessMove
	shl ebx, 2						; 2^2 = 4
	sub edi, ebx					; moveArray.get(len-1-4)

	mov cl, 4
	START_CHECK:
		; Get Moves
		mov eax, (ChessMove PTR [esi]).moveCoords	; current
		mov ebx, (ChessMove PTR [edi]).moveCoords	; current - 4

		cmp eax, ebx
		jne SKIP					; If moves aren't equal, not repetition

		; Move pointers back one previous move
		sub esi, SIZEOF ChessMove
		sub edi, SIZEOF ChessMove
		dec cl
	jnz START_CHECK

	test eax, 0 		; Set zero flag

	SKIP:
	ret
CheckThreefoldRepetition ENDP

END