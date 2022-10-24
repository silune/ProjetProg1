open Format
open X86_64
open Parser


let push_int r = pushq (reg r);;
let pop_int r = popq r;;
let push_float r = (subq (imm 8) (reg rsp)) ++ (movsd (reg r) (ind rsp));;
let pop_float r = (movsd (ind rsp) (reg r)) ++ (addq (imm 8) (reg rsp));;

let code_of_op op =
        match op with
        | INT x -> movq (imm (int_of_string x)) (reg rdi) ++ (push_int rdi)
        | ADDI _ -> (pop_int rdi) ++ (pop_int rsi) ++ (addq (reg rdi) (reg rsi)) ++ (push_int rsi)
        | SUBI _ -> (pop_int rdi) ++ (pop_int rsi) ++ (subq (reg rdi) (reg rsi)) ++ (push_int rsi)
        | MULI _ -> (pop_int rdi) ++ (pop_int rsi) ++ (imulq (reg rdi) (reg rsi)) ++ (push_int rsi)
        | DIVI _ -> (pop_int rax) ++ (pop_int rdi) ++ (movq (imm 0) (reg rdx)) ++ (idivq (reg rdi)) ++ (push_int rax)
        | MODI _ -> (pop_int rax) ++ (pop_int rdi) ++ (movq (imm 0) (reg rdx)) ++ (idivq (reg rdi)) ++ (push_int rdx)
        | NEGI _ -> (pop_int rdi) ++ (negq (reg rdi)) ++ (push_int rdi)
        | _ -> failwith "not implemented in assembly yet";;

let rec code_of_tree tree floatCount =
        match tree with
        | INT x -> (code_of_op tree, floatCount)
        | ADDI (t1, t2) | SUBI (t1, t2) | MULI (t1, t2) | DIVI (t1, t2) | MODI (t1, t2) -> 
                        let (subCode1, subFloatCount1) = (code_of_tree t1 floatCount) in
                        let (subCode2, subFloatCount2) = (code_of_tree t2 subFloatCount1) in
                                (subCode1 ++ subCode2 ++ (code_of_op tree), subFloatCount2)
        | NEGI t -> let (subCode, subFloatCount) = (code_of_tree t floatCount) in
                                (subCode ++ (code_of_op tree), subFloatCount)
        | _ -> failwith "not implemented in assembly yet";;

let print_int_fun =
        label "print_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab "S_int") (reg rdi) ++
        xorq (reg rax) (reg rax) ++
        call "printf" ++
        ret

let assembly_of_tree tree =
        let (codeTree, floatCount) = code_of_tree tree 0 in
        let code = {text =
                globl "main" ++ label "main" ++
                pushq (reg rbp) ++
                movq (reg rsp) (reg rbp) ++
                codeTree ++
                (pop_int rdi) ++
                movq (reg rbp) (reg rsp) ++
                popq rbp ++
                call "print_int" ++
                ret ++
                print_int_fun;
                data =
                        label "S_int" ++ string "%d \n";} in
        let c = open_out "test.s" in
        let fmt = formatter_of_out_channel c in
        X86_64.print_program fmt code;
        close_out c;;

assembly_of_tree (ADDI (INT "3", SUBI (INT "5", MULI (NEGI (INT "2"), DIVI (INT "4", MODI (INT "10", INT "7"))))));;
