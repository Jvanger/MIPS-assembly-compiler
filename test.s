	.data
	__nl: .asciiz "\n"
	.text
	.globl main
	.text

# minus function
_minus:
	sw    $ra, 0($sp)	# save return address
	subu  $sp, $sp, 4
	sw    $fp, 0($sp)	# save frame pointer
	subu  $sp, $sp, 4
	addu  $fp, $sp, 8
	subu  $sp, $sp, 4		# allocate space for locals
	addi  $t0, $fp, -8		# compute address of local/param: m
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($fp)	# push value of local/param: x
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 8($fp)	# push value of local/param: y
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sub   $t0, $t0, $t1		# subtract operands
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t0, 0($t1)	# store value at address
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t0, -8($fp)	# push value of local/param: m
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $v0, 4($sp)	# POP
	addu  $sp, $sp, 4
	j     _minus_exit		# jump to function exit

# minus function epilogue
_minus_exit:
	lw    $ra, 0($fp)	# restore return address
	move  $t0, $fp		# save control link
	lw    $fp, -4($fp)	# restore frame pointer
	move  $sp, $t0		# restore SP
	jr    $ra		# return to caller

# main function
main:
	sw    $ra, 0($sp)	# save return address
	subu  $sp, $sp, 4
	sw    $fp, 0($sp)	# save frame pointer
	subu  $sp, $sp, 4
	addu  $fp, $sp, 8
	subu  $sp, $sp, 12		# allocate space for locals
	addi  $t0, $fp, -8		# compute address of local/param: a
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	li    $t0, 7		# push integer literal: 7
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t0, 0($t1)	# store value at address
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	addi  $t0, $fp, -12		# compute address of local/param: b
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, -8($fp)	# push value of local/param: a
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t0, 0($t1)	# store value at address
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	addi  $t0, $fp, -16		# compute address of local/param: c
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, -8($fp)	# push value of local/param: a
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, -12($fp)	# push value of local/param: b
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	add   $t0, $t0, $t1		# add operands
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t0, 0($t1)	# store value at address
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t0, -8($fp)	# push value of local/param: a
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, -12($fp)	# push value of local/param: b
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	mult  $t0, $t1		# multiply operands
	mflo  $t0		# get lower 32 bits of result
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)	# load int value
	addu  $sp, $sp, 4
	li    $v0, 1
	syscall		# print integer
	la    $a0, __nl		# load newline string
	li    $v0, 4
	syscall		# print newline
	lw    $t0, -16($fp)	# push value of local/param: c
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, -8($fp)	# push value of local/param: a
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	add   $t0, $t0, $t1		# add operands
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)	# load int value
	addu  $sp, $sp, 4
	li    $v0, 1
	syscall		# print integer
	la    $a0, __nl		# load newline string
	li    $v0, 4
	syscall		# print newline
	addi  $t0, $fp, -16		# compute address of local/param: c
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, -16($fp)	# push value of local/param: c
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	li    $t0, 2		# push integer literal: 2
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	mult  $t0, $t1		# multiply operands
	mflo  $t0		# get lower 32 bits of result
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t0, 0($t1)	# store value at address
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t0, -16($fp)	# push value of local/param: c
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)	# load int value
	addu  $sp, $sp, 4
	li    $v0, 1
	syscall		# print integer
	la    $a0, __nl		# load newline string
	li    $v0, 4
	syscall		# print newline
	addi  $t0, $fp, -12		# compute address of local/param: b
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, -16($fp)	# push value of local/param: c
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, -8($fp)	# push value of local/param: a
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	jal   _minus		# call function: minus
	addu  $sp, $sp, 8		# clean up parameters
	sw    $v0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t1, 4($sp)	# POP
	addu  $sp, $sp, 4
	sw    $t0, 0($t1)	# store value at address
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $t0, 4($sp)	# POP
	addu  $sp, $sp, 4
	lw    $t0, -12($fp)	# push value of local/param: b
	sw    $t0, 0($sp)	# PUSH
	subu  $sp, $sp, 4
	lw    $a0, 4($sp)	# load int value
	addu  $sp, $sp, 4
	li    $v0, 1
	syscall		# print integer
	la    $a0, __nl		# load newline string
	li    $v0, 4
	syscall		# print newline

# main function epilogue
_main_Exit:
	lw    $ra, 0($fp)	# restore return address
	move  $t0, $fp		# save control link
	lw    $fp, -4($fp)	# restore FP
	move  $sp, $t0		# restore SP
	li    $v0, 10		# load exit code for syscall
	syscall		# only do this for main
