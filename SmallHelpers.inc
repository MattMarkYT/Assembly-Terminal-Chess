COMMENT !

List of Procedures:

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

YesOrNo PROTO
NormalizeVectors PROTO
GetCoordsDifference PROTO