COMMENT !

SetMoveCoords PROTO
	-- Sets current ChessMove move coordinates
	-- Input: edx (move coordinates y2-x2-y1-x1)

SetCapturedPiece PROTO
	-- Sets current ChessMove captured piece
	-- Input: edi (captured piece)

SetMovedPieceBefore PROTO
	-- Sets current ChessMove captured piece
	-- Input: esi (moving piece)

SetMovedPieceAfter PROTO
	-- Sets current ChessMove captured piece
	-- Input: edi (moved piece)

IncrementChessHistory PROTO
	-- Increments length to point to the next ChessMove

ResetChessHistory PROTO
	-- Resets length to 0, clearing history

GetChessHistoryLength PROTO
	-- Get the current value of length
	-- Output: eax (length)

CheckThreefoldRepetition PROTO
	-- Checks history to see if threefold repetition happened
	-- Output: Zero flag (1 if true, 0 if false)

END COMMENT !

SetMoveCoords PROTO
SetCapturedPiece PROTO
SetMovedPieceBefore PROTO
SetMovedPieceAfter PROTO
IncrementChessHistory PROTO
ResetChessHistory PROTO
GetChessHistoryLength PROTO
CheckThreefoldRepetition PROTO