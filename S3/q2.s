	.text
	.globl	main
main:
	movq	$54, %rdi
	movq	$57, %rsi
	call	add_int
	movq	%rax, %rdi
	call	print_int
	movq	$0, %rax
	ret

add_int:
	addq	%rdi, %rsi
	movq	%rsi, %rax
	ret

print_int:
	movq	%rdi, %rsi
	movq	$message, %rdi
	call 	printf
	movq	$0, %rax
	ret

	.data
message:
	.string "%d \n"
