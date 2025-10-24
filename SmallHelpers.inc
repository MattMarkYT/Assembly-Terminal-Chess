COMMENT !

List of Procedures:

MoveSPointerToSquare PROTO
	-- Takes chess input in dx (dh is row, dl is column), esi starting address of chessboard
	-- and sets esi to the square

MoveDPointerToSquare PROTO
	-- Takes chess input in dx (dh is row, dl is column), edi starting address of chessboard
	-- and sets edi to the square

MoveDXToKing PROTO
	-- Takes esi starting address of chessboard, ah as GAME_STATUS's IS_BLACK_TURN bit
	-- and sets dx to king square coords depending on GAME_STATUS
	-- dl: column, dh: row

YesOrNo PROTO
	-- Takes character in al and checks if it's y or n
	-- sets carry flag if al is 'y' or 'Y'
	-- sets zero flag if al is 'n' or 'N'
	-- Usage: After calling, check carry flag before zero flag

NormalizeVectors PROTO
	-- Takes in al(x) and ah(y) and normalizes them
	-- Turns to 1 if positive, -1 if negative, 0 if zero

GetCoordsDifference PROTO
	-- takes the coords in edx and returns the difference
	-- al is difference in x, cl is the absolute value
	-- ah is difference in y, ch is the absolute value

END COMMENT !

MoveSPointerToSquare PROTO
MoveDPointerToSquare PROTO
MoveDXToKing PROTO
YesOrNo PROTO
NormalizeVectors PROTO
GetCoordsDifference PROTO