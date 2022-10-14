	.text
	.globl	main
main:
	movsd	nombre1, %xmm0
	movsd	nombre2, %xmm1
	movq	$1, %rax
	call	add_float
	call	print_float
	movq	$0, %rax
	ret

nombre1:
	.double	0.2

nombre2:
	.double	0.22

add_int:
	addq	%rdi, %rsi
	movq	%rsi, %rax
	ret

add_float:
	addsd	%xmm1, %xmm0
	movq	$0, %rax
	ret

print_int:
	movq	%rdi, %rsi
	movq	$message_int, %rdi
	call 	printf
	movq	$0, %rax
	ret

print_float:
	movq	$message_float, %rdi
	movq	$1, %rax
	call	printf
	movq	$0, %rax
	ret

	.data
message_int:
	.string "%d \n"

	.data
message_float:
	.string "%f \n"
