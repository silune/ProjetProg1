open Format
open X86_64
open Parser


let push_int r = pushq (reg r);;
let pop_int r = popq r;;
let push_float r = (subq (imm 8) (reg rsp)) ++ (movsd (reg r) (ind rsp));;
let pop_float r = (movsd (ind rsp) (reg r)) ++ (addq (imm 8) (reg rsp));;

let code_of_op op =
        match op with
        | ADDI _ -> (pop_int rdi) ++ (pop_int rsi) ++ (addq (reg rdi) (reg rsi)) ++ (push_int rsi)
        | SUBI _ -> (pop_int rdi) ++ (pop_int rsi) ++ (subq (reg rdi) (reg rsi)) ++ (push_int rsi)
        | MULI _ -> (pop_int rdi) ++ (pop_int rsi) ++ (imulq (reg rdi) (reg rsi)) ++ (push_int rsi)
        | DIVI _ -> (pop_int rax) ++ (pop_int rdi) ++ (movq (imm 0) (reg rdx)) ++ (idivq (reg rdi)) ++ (push_int rax)
        | MODI _ -> (pop_int rax) ++ (pop_int rdi) ++ (movq (imm 0) (reg rdx)) ++ (idivq (reg rdi)) ++ (push_int rdx)
        | NEGI _ -> (pop_int rdi) ++ (negq (reg rdi)) ++ (push_int rdi)
        | ADDF _ -> (pop_float xmm0) ++ (pop_float xmm1) ++ (addsd (reg xmm0) (reg xmm1)) ++ (push_float xmm1)
        | _ -> failwith "not implemented in assembly yet";;

let rec code_of_tree tree floatCount floatLabel =
        match tree with
        | INT x ->      (movq (imm (int_of_string x)) (reg rdi) ++ (push_int rdi),
                        floatCount,
                        floatLabel)
        | FLOAT x ->    (movsd (lab ("f"^(string_of_int floatCount))) (reg xmm0) ++ (push_float xmm0),
                        floatCount + 1,
                        floatLabel ++ label ("f"^(string_of_int floatCount)) ++ double (float_of_string x))
        | ADDI (t1, t2) | SUBI (t1, t2) | MULI (t1, t2) | DIVI (t1, t2) | MODI (t1, t2) | ADDF (t1, t2) -> 
                let (subCode1, subFloatCount1, subFloatLabel1) = (code_of_tree t1 floatCount floatLabel) in
                let (subCode2, subFloatCount2, subFloatLabel2) = (code_of_tree t2 subFloatCount1 subFloatLabel1) in
                        (subCode1 ++ subCode2 ++ (code_of_op tree),
                        subFloatCount2, 
                        subFloatLabel2)
        | NEGI t -> let (subCode, subFloatCount, subFloatLabel) = (code_of_tree t floatCount floatLabel) in
                                (subCode ++ (code_of_op tree),
                                subFloatCount,
                                subFloatLabel)
        | _ -> failwith "not implemented in assembly yet";;

let print_int_fun =
        label "print_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab "S_int") (reg rdi) ++
        xorq (reg rax) (reg rax) ++
        call "printf" ++
        ret

let print_float_fun =
        label "print_float" ++
        movq (ilab "S_float") (reg rdi) ++
        movq (imm 1) (reg rax) ++
        call "printf" ++
        movq (imm 0) (reg rax) ++
        ret

let init_stack = pushq (reg rbp) ++ movq (reg rsp) (reg rbp);;
let end_stack = movq (reg rbp) (reg rsp) ++ popq rbp;;

let print_res tree =
        if type_of_tast tree = "int"
                then (pop_int rdi) ++ end_stack ++ call "print_int"
                else (pop_float xmm0) ++ end_stack ++ call "print_float";;

let assembly_of_tree tree =
        let (codeTree, _, floatLabel) = code_of_tree tree 0 nop in
        let code = {text =
                globl "main" ++ label "main" ++
                init_stack ++
                codeTree ++
                (print_res tree) ++
                ret ++
                print_int_fun ++
                print_float_fun;
                data =
                        label "S_int" ++ string "%d \n" ++
                        label "S_float" ++ string "%f \n" ++ 
                        floatLabel;} in
        let c = open_out "test.s" in
        let fmt = formatter_of_out_channel c in
        X86_64.print_program fmt code;
        close_out c;;

assembly_of_tree (ADDF (FLOAT "0.2", FLOAT "0.22"));;
