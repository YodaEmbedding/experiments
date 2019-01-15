**ARM Assembly project for ENSC 254**

ZedBoard  
IC: Xilinx Zynq®-7000 All Programmable SoC  
Processor type: ARM

### Subroutines/functions

#### arm.s

	asm_main
	checkCollision
	displayGameOver
	displayWelcome
	generateDisplayString
	generateEnemy
	getEnemyPositionAtOffset
	getRandom
	getTick
	memclear
	moveEnemies
	movePlayer
	syncFPS
	updateButtonState
	updateDisplay
	updatePositions
	updateScoreAndSpeed
	waitCycles

#### display.s

	div10
	OLED_print_char
	OLED_print_string
	OLED_print_uint
