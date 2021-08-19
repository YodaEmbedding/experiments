.section .data
.section .text

//Define the base addresses of the GPIO ports we might want to use.
.align		//align the variable to 4 bytes word boundary
GpioBTN:	.word 0x41200000
GpioLED:	.word 0x41210000
GpioSW: 	.word 0x41220000
GpioOLED:	.word 0x41230000

OLEDController:	.word 0x41230000

.global asm_main   // make asm_main visible to the linker program

.func asm_main
asm_main:
	push {lr}

	bl displayWelcome

	gameInit:
		ldr r0, =playerPosition
		mov r1, #0
		str r1, [r0]

		ldr r0, =score
		mov r1, #0
		str r1, [r0]

		ldr r0, =speed
		mov r1, #SPEED_MAX
		str r1, [r0]

		ldr r0, =enemyPositionsCurrIdx
		mov r1, #0
		str r1, [r0]

		ldr r0, =displayString
		mov r1, #DISPLAY_STRING_SIZE
		mov r2, #0
		bl memclear

		ldr r0, =enemyPositions
		mov r1, #ENEMY_POSITIONS_SIZE
		mov r2, #ENEMY_NONE
		bl memclear

	gameLoop:
		bl checkCollision
		bl updateButtonState
		bl updatePositions
		bl updateDisplay
		bl syncFPS

		b gameLoop

	gameOver:
		pop {lr} // due to premature exit
		bl displayGameOver
		b gameInit

	pop {lr}
	bx lr  // Return from function call
.endfunc


displayWelcome:
	push {lr}

	ldr r0, =welcomeString
	mov r1, #0
	mov r2, #0
	bl OLED_print_string

	mov r0, #WELCOME_WAIT
	bl waitCycles

	pop {lr}
	mov pc, lr


displayGameOver:
	push {lr}

	ldr r0, =gameOverString
	mov r1, #0
	mov r2, #0
	bl OLED_print_string

	ldr r0, =score
	ldr r0, [r0]
	mov r1, #GAME_OVER_SCORE_X
	mov r2, #GAME_OVER_SCORE_Y
	bl OLED_print_uint

	mov r0, #GAME_OVER_WAIT
	bl waitCycles

	pop {lr}
	mov pc, lr


/*
	if (playerPosition == enemyPosition(0))
		goto gameOver;
*/
checkCollision:
	push {lr}

	mov r0, #0
	bl getEnemyPositionAtOffset

	ldr r1, =playerPosition
	ldr r1, [r1]

	cmp r0, r1
	beq gameOver // minor memory leak

	cmp r0, #ENEMY_NONE
	beq checkCollision_return

	// Successful avoidance
	bl updateScoreAndSpeed

	checkCollision_return:
	pop {lr}
	mov pc, lr


updateScoreAndSpeed:
	// Increase score
	ldr r0, =score
	ldr r1, [r0]
	add r1, #1
	str r1, [r0]

	// Update speed
	ldr r0, =speed
	ldr r1, [r0]
	cmp r1, #SPEED_MIN
	subgt r1, #1
	str r1, [r0]

	mov pc, lr


// Update buttonState
updateButtonState:
	//!todo
	ldr r0, =GpioBTN
	ldr r0, [r0]
	ldr r0, [r0]
	ldr r1, =buttonState
	str r0, [r1]
	mov pc, lr


updatePositions:
	push {lr}

	bl movePlayer
	bl generateEnemy
	bl moveEnemies

	pop {lr}
	mov pc, lr


/*
	if(buttonState & BUTTON_LEFT && playerPosition > 0)
		playerPosition--;

	if(buttonState & BUTTON_RIGHT && playerPosition < SCREEN_ROWS - 1)
		playerPosition++;
*/
movePlayer:
	ldr r0, =buttonState
	ldr r0, [r0]

	ldr r2, =playerPosition
	ldr r3, [r2]

	movePlayer_left:
		tst r0, #BUTTON_LEFT
		beq movePlayer_right
		cmp r3, #POSITION_MIN
		ble movePlayer_right

		sub r3, #1
		str r3, [r2]

	movePlayer_right:
		tst r0, #BUTTON_RIGHT
		beq movePlayer_return
		cmp r3, #POSITION_MAX
		bge movePlayer_return

		add r3, #1
		str r3, [r2]

	movePlayer_return:
	mov pc, lr


/*
	enemyPositionsCurrIdx++;
	if(enemyPositionsCurrIdx >= ENEMIES_MAX)
		enemyPositionsCurrIdx = 0;
*/
moveEnemies:
	ldr r0, =enemyPositionsCurrIdx
	ldr r1, [r0]
	add r1, #1

	cmp r1, #ENEMIES_MAX
	movge r1, #0

	str r1, [r0]

	mov pc, lr


/*
	enemyPositions[enemyPositionsCurrIdx] = getRandom() % SCREEN_ROWS;
*/
generateEnemy:
	push {r4-r6, lr}

	ldr r4, =enemyPositionsCurrIdx
	ldr r4, [r4]

	ldr r5, =enemyPositions

	ldr r6, =enemiesSkipped
	ldr r1, [r6]

	cmp r1, #0
	ble generateEnemy_new

	generateEnemy_none:
		sub r1, #1
		str r1, [r6]

		mov r6, #ENEMY_NONE
		strb r6, [r5, r4]

		b generateEnemy_return

	generateEnemy_new:
		// Reset skip count
		bl getRandom
		and r0, #ENEMY_SKIP_RANDOM_MASK
		add r0, #ENEMY_SKIP_MIN
		str r0, [r6]

		// Generate position
		bl getRandom
		and r0, #ENEMY_POSITION_RANDOM_MASK
		strb r0, [r5, r4]

	generateEnemy_return:
	pop {r4-r6, lr}
	mov pc, lr


/*
xorshift
http://excamera.com/sphinx/article-xorshift.html

  seed ^= seed << 13;
  seed ^= seed >> 17;
  seed ^= seed << 5;
  return seed;

Return values:
	r0: random number between 0 and 0xFFFFFFFF
*/
getRandom:
	ldr r1, =seed
	ldr r0, [r1]
	eor r0, r0, lsl #13
	eor r0, r0, lsr #17
	eor r0, r0, lsl #5
	str r0, [r1]
	// bl MT_Extract
	mov pc, lr


updateDisplay:
	push {lr}

	bl generateDisplayString

	ldr r0, =displayString
	mov r1, #0
	mov r2, #0
	bl OLED_print_string

	ldr r0, =score
	ldr r0, [r0]
	mov r1, #SCORE_X
	mov r2, #SCORE_Y
	bl OLED_print_uint

	pop {lr}
	mov pc, lr


/*
Input:
	r0: address of memory block
	r1: size of memory block in bytes
	r2: default byte value
*/
memclear:
	memclear_loop:
		cmp r1, #0
		ble memclear_return
	
		strb r2, [r0], #1
		sub r1, #1
		b memclear_loop

	memclear_return:
	mov pc, lr


/*
Input:
	r0: Offset
Output:
	r0: enemyPositions[(enemyPositionsCurrIdx + Offset) % ENEMIES_MAX]
*/
getEnemyPositionAtOffset:
	ldr r1, =enemyPositions
	ldr r2, =enemyPositionsCurrIdx
	ldr r2, [r2]
	add r3, r0, r2
	cmp r3, #ENEMIES_MAX
	subge r3, #ENEMIES_MAX
	ldrb r0, [r1, r3]
	mov pc, lr


generateDisplayString:
	push {r4-r7, lr}
	ldr r4, =displayString
	ldr r5, =playerPosition
	ldr r5, [r5]

	// Clear display string with spaces
	mov r0, r4
	mov r1, #DISPLAY_STRING_LENGTH
	mov r2, #CHAR_SYMBOL_EMPTY
	bl memclear

	// Ensure null-terminated string
	mov r0, #0
	add r1, r4, #DISPLAY_STRING_LENGTH
	strb r0, [r1]

	mov r7, #CHAR_SYMBOL_ENEMY
	mov r6, #0
	generateDisplayString_enemiesLoop:
		mov r0, r6
		bl getEnemyPositionAtOffset

		mov r3, #SCREEN_COLUMNS
		mul r3, r0
		add r3, r6

		cmp r0, #ENEMY_NONE
		beq generateDisplayString_noEnemy
		strb r7, [r4, r3]		// Write enemy character

		generateDisplayString_noEnemy:
		add r6, #1
		cmp r6, #ENEMIES_MAX
		blt generateDisplayString_enemiesLoop

	// Draw player
	// Check if enemyPosition(0) == playerPosition
	mov r0, #0
	bl getEnemyPositionAtOffset
	mov r7, #CHAR_SYMBOL_PLAYER
	cmp r0, r5
	moveq r7, #CHAR_SYMBOL_EXPLOSION

	mov r3, #SCREEN_COLUMNS
	mul r3, r5
	strb r7, [r4, r3]

	pop {r4-r7, lr}
	mov pc, lr


/*

Input:
	r0: cycles
*/
waitCycles:
	waitCycles_loop:
		subs r0, #1
		bne waitCycles_loop
	mov pc, lr


/*
	syncTick = syncTick + WAIT_TIME;
	while (getTick() < syncTick);
*/
/*
syncFPS:
	push {r4, lr}

	ldr r0, =WAIT_TIME
	ldr r1, =syncTick
	ldr r4, [r1]
	add r4, r0
	str r4, [r1]

	syncFPS_loop:
		bl getTick
		cmp r0, r4
		blt syncFPS_loop

	pop {r4, lr}
	mov pc, lr
*/
syncFPS:
	push {lr}

	ldr r0, =speed
	ldr r0, [r0]
	mov r1, #WAIT_CYCLE_LENGTH
	mul r0, r1
	bl waitCycles
	
	pop {lr}
	mov pc, lr


/*
Return values:
	r0: system timer
*/
getTick:
	//!todo
	mov pc, lr


.equ BUTTON_CENTER, 0x01		// #(1 << 0)
.equ BUTTON_LEFT, 0x04		 	// #(1 << 2)
.equ BUTTON_RIGHT, 0x08			// #(1 << 3)

.equ CHAR_SYMBOL_EMPTY, ' '
.equ CHAR_SYMBOL_ENEMY, 'X'
.equ CHAR_SYMBOL_EXPLOSION, '+'
.equ CHAR_SYMBOL_PLAYER, '>'

.equ DISPLAY_STRING_LENGTH, 64	// SCREEN_ROWS * SCREEN_COLUMNS
.equ DISPLAY_STRING_SIZE, 65	// DISPLAY_STRING_LENGTH + 1

.equ ENEMIES_MAX, 16
.equ ENEMY_NONE, 0xFF
.equ ENEMY_POSITIONS_SIZE, 64	// ENEMIES_MAX * 4
.equ ENEMY_POSITION_RANDOM_MASK, 0b11
.equ ENEMY_SKIP_MIN, 3
.equ ENEMY_SKIP_RANDOM_MASK, 0b11

.equ POSITION_MAX, 3			// SCREEN_ROWS - 1
.equ POSITION_MIN, 0

.equ SCORE_X, 12
.equ SCORE_Y, 3

.equ SCREEN_COLUMNS, 16
.equ SCREEN_ROWS, 4

.equ SPEED_MAX, 60
.equ SPEED_MIN, 5

.equ WAIT_TIME, 100
.equ WAIT_CYCLE_LENGTH, 0x00100000

.equ WELCOME_STRING_SIZE, 65
.equ WELCOME_WAIT, 0xF0000000

.equ GAME_OVER_STRING_SIZE, 65
.equ GAME_OVER_SCORE_X, 8
.equ GAME_OVER_SCORE_Y, 3
.equ GAME_OVER_WAIT, 0x80000000


.data
	.align
	buttonState: .space 1
	.align
	displayString: .space DISPLAY_STRING_SIZE
	.align
	welcomeString: .asciz	"                   ARM RACING   BY MATEEN ULHAQ                 "
	.align
	gameOverString: .asciz	"  YOU CRASHED!                  SCORE:                          "
	.align
	enemiesSkipped: .space 4
	enemyPositions: .space ENEMY_POSITIONS_SIZE
	.align
	enemyPositionsCurrIdx: .space 4
	playerPosition: .space 4
	score: .space 4
	speed: .space 4
	syncTick: .space 4

	seed: .word 314159265

.end


/*
Todo:

Playability:
	WAIT_TIME speeds up as game progresses
	WELCOME screen
	Gameover instantly restarts?
	To make playable ensure not impossible?

Sprites:
	.set OLED_FONT_OFFSET, 0x10000
	// offset from OLED base address to font table buffer

*/
