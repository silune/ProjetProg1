	.text
	.globl	main
main:
	pushq 	%rbp
	movq 	%rsp, %rbp
	movq 	$3, %rdi
	pushq 	%rdi
	movq 	$5, %rdi
	pushq 	%rdi
	movq 	$2, %rdi
	pushq 	%rdi
	popq 	%rdi
	negq 	%rdi
	pushq 	%rdi
	movq 	$4, %rdi
	pushq 	%rdi
	movq 	$10, %rdi
	pushq 	%rdi
	movq 	$7, %rdi
	pushq 	%rdi
	popq 	%rax
	popq 	%rdi
	movq 	$0, %rdx
	idivq 	%rdi
	pushq 	%rdx
	popq 	%rax
	popq 	%rdi
	movq 	$0, %rdx
	idivq 	%rdi
	pushq 	%rax
	popq 	%rdi
	popq 	%rsi
	imulq 	%rdi, %rsi
	pushq 	%rsi
	popq 	%rdi
	popq 	%rsi
	subq 	%rdi, %rsi
	pushq 	%rsi
	popq 	%rdi
	popq 	%rsi
	addq 	%rdi, %rsi
	pushq 	%rsi
	popq 	%rdi
	movq 	%rbp, %rsp
	popq 	%rbp
	call 	print_int
	ret
print_int:
	movq 	%rdi, %rsi
	movq 	$S_int, %rdi
	xorq 	%rax, %rax
	call 	printf
	ret
	.data
S_int:
	.string "%d \n"
