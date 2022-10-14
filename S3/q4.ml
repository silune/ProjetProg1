let add_float_assembly_text x y = "	
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
	.double	" ^ (Float.to_string x) ^ "

nombre2:
	.double	" ^ (Float.to_string y) ^ "

add_float:
	addsd	%xmm1, %xmm0
	movq	$0, %rax
	ret

print_float:
	movq	$message_float, %rdi
	movq	$1, %rax
	call	printf
	movq	$0, %rax
	ret

	.data
message_float:
	.string \"%f \\n\"";;

let add_float_assembly_file x y =
        let oc = open_out "addFromCaml.s" in
        Printf.fprintf oc "%s\n"  (add_float_assembly_text x y);
        close_out oc;;
