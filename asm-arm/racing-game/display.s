.section .text

GpioOLED:	.word 0x41230000

.set OLED_COLS, 16				// Number of column on the display
.set OLED_FONT_OFFSET, 0x10000	// Offset from OLED base address to font table buffer
.set OLED_CHAR_OFFSET, 0x10400	// Offset from OLED base address to character display buffer


/*
Inputs:
	r0: character
	r1: x
	r2: y
*/
.global OLED_print_char
.func OLED_print_char
OLED_print_char:
	push {r4, lr}

	add r3, r1, r2, lsl #4			// Calculate character offset

	// Determine start address
	ldr r4, =GpioOLED
	ldr r4, [r4]					// Load base address for OLED
	add r4, #OLED_CHAR_OFFSET		// Go to OLED character buffer
	add r4, r3						// Move to correct position

	strb r0, [r4]					// Print character

	pop {r4, pc}
.endfunc


/*
Inputs:
	r0: address of null-terminated string
	r1: x
	r2: y
*/
.global OLED_print_string
.func OLED_print_string
OLED_print_string:
	push {r4, r5, lr}

	add r3, r1, r2, lsl #4			// Calculate character offset

	// Determine start address
	ldr r4, =GpioOLED
	ldr r4, [r4]					// Load base address for OLED
	add r4, #OLED_CHAR_OFFSET		// Go to OLED character buffer
	add r4, r3						// Move to correct position

	OLED_print_string_loop:
		ldrb r5, [r0], #1			// Get character
		cmp r5, #0					// Check for null-terminator
		beq OLED_print_string_loop_exit

		strb r5, [r4], #1			// Print character
		b OLED_print_string_loop
	OLED_print_string_loop_exit:

	pop {r4, r5, pc}
.endfunc


;@ subroutine to divide number in r0 by 10
div10:
	;@ r0 should contain the dividend
	push {r11, lr}
	ldr		r1, =0xCCCCCCCD		;@ fix point binary representation of 1/10
	umull	r2, r1, r0, r1
	mov		r0, r1, LSR #3		;@ shifting the result of the multiplication by 32+3 bits
								;@ will give the division result
	pop {r11, pc}

;@ Sub routine to display an integer number on a specific row of the OLED screen
;@ the number displayed will be considered unsigned
;@ argument: 	r0 should contain the number to be display upon entering the sub routine
;@ 				r1 holds the column number on the OLED screen to be displayed
;@ 				r2 holds the row number on the OLED screen to be displayed
counter_str:	.space 16	;@ space to store the OLED display string
.global OLED_print_uint
.func OLED_print_uint
OLED_print_uint:
	push {r4-r10, r11, r12, lr}

;@	ldr r5, =counter
;@	ldr r5, [r5]
	mov r5, r0					;@ use r5 to store the value to display
	mov r11, r1					;@ store specified column number in r12
	mov r12, r2					;@ store specified row number in r12

	ldr r8, =counter_str		;@ use r8 as pointer to counter string
	mov r6, #0					;@ pad a NULL at the beginning
	strb r6, [r8], #1
	div10_get_remainder_loop:
	mov r0, r5
	bl div10					;@ divide by 10
	mov r10, r0					;@ move division result from r0 to r10
	mov r6, #10
	umull r7, r6, r10, r6
	sub r9, r5, r7				;@ and get the remainder (this will be the least significant digit)
	add r9, r9, #'0'			;@ add the value to ascii equivalent of number 0 (0x30)
	strb r9, [r8], #1			;@ store the byte to counter string
	mov r5, r10					;@ store division result back to r5
	cmp r10, #0					;@ if division result is not 0, repeat
	bne	div10_get_remainder_loop

	;@ display the counter string on OLED:
	ldr r4, =GpioOLED
	ldr r4, [r4]					;@ load base address for OLED
	add r4, r4, #OLED_CHAR_OFFSET	;@ go to OLED character buffer
	add r6, r4, r12, LSL #4			;@ point r6 to the beginning of the specified row on OLED
	add r6, r11						;@ point r6 to the beginning of the specified columnb on OLED
	sub r8, r8, #1
	;@ at this point r8 should point at the end of the string and also the most significant digit of the counter
	print_counter_OLED_loop:
	ldrb r5, [r8], #-1			;@ load character from the string in backward direction
	cmp r5, #0					;@ stop when reaching NULL at the beginning of the counter string
	beq	print_counter_OLED_loop_stop
	strb r5, [r6], #1			;@ store character on OLED display buffer
	b print_counter_OLED_loop
	print_counter_OLED_loop_stop:

	;@ clean up the remaining of the line with white space
	add r12, r12, #1			;@ Specify the end of the specified row (beginning of next row)
	print_counter_OLED_clean:
	mov r5, #' '
	strb r5, [r6], #1
	sub r7, r6, r4				;@ get current position of r6 pointer on OLED buffer (r7 represent offset from base addr)
				;@ compare position on OLED buffer with
	cmp r7, r12, LSL #4			;@ the first character of the specified row + 1
								;@ (make sure its past the last character of the row)
	blt	print_counter_OLED_clean


	pop {r4-r10, r11, r12, pc}
.endfunc ;@ OLED_print_uint
