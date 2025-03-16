PS2_SCANCODES:
		DB 0EH, '`', '~', 00H
		DB 16H, '1', '!', 00H
		DB 1EH, '2', '@', 00H
		DB 26H,	'3', '#', 00H
		DB 25H,	'4', '$', 00H
		DB 2EH,	'5', '%', 00H
		DB 36H, '6', '^', 00H
		DB 3DH,	'7', '&', 00H
		DB 3EH, '8', '*', 00H
		DB 46H, '9', '(', 00H
		DB 45H,	'0', ')', 00H
		DB 4EH, '-', '_', 00H
		DB 55H, '=', '+', 00H
		DB 66H, 08H, 08H, 00H			;Bacspace here!!!!
		DB 0DH, 09H, 09H, 00H				;TAB here!!!!!
		DB 15H, 'q', 'Q', 11H
		DB 1DH, 'w', 'W', 17H
		DB 24H, 'e', 'E', 05H
		DB 2DH, 'r', 'R', 12H
		DB 2CH, 't', 'T', 14H
		DB 35H, 'y', 'Y', 19H
		DB 3CH, 'u', 'U', 15H
		DB 43H, 'i', 'I', 09H
		DB 44H, 'o', 'O', 0FH
		DB 4DH, 'p', 'P', 10H
		DB 54H, '[', '{', 1BH
		DB 5BH, ']', '}', 1DH
		DB 58H, 00H, 00H, 00H				;CAPSLOCK here!!!!
		DB 1CH, 'a', 'A', 01H
		DB 1BH, 's', 'S', 13H
		DB 23H, 'd', 'D', 04H
		DB 2BH, 'f', 'F', 06H
		DB 34H, 'g', 'G', 07H
		DB 33H, 'h', 'H', 08H
		DB 3BH, 'j', 'J', 0AH
		DB 42H, 'k', 'K', 0BH
		DB 4BH, 'l', 'L', 0CH
		DB 4CH, ';', ':', 00H
		DB 52H, 27H, 22H, 00H				; ' and "
		DB 5AH, 0DH, 0DH, 00H				;ENTER here!!!!!
		DB 1AH, 'z', 'Z', 1AH
		DB 22H, 'x', 'X', 18H
		DB 21H, 'c', 'C', 03H
		DB 2AH, 'v', 'V', 16H
		DB 32H, 'b', 'B', 02H
		DB 31H, 'n', 'N', 0EH
		DB 3AH, 'm', 'M', 0DH
		DB 41H, ',', '<', 00H
		DB 49H, '.', '>', 00H
		DB 4AH, '/', '?', 00H
		DB 29H, ' ', ' ', 00H
		DB 76H, 03H, 03H, 03H				;ESC = Ctrl+C
PS2_SCANCODES_END: 
