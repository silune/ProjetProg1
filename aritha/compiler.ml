open Format
open X86_64
open Parser

(* Compiler : converts a tast into program type of the X86_64 lib *)

(* ----- Auxilary functions ----- *)

(* -- code constants -- *)

(* push / pop registers in the stack *)
let push_int r = pushq (reg r)
let pop_int r = popq r
let push_float r = (subq (imm 8) (reg rsp)) ++ (movsd (reg r) (ind rsp))
let pop_float r = (movsd (ind rsp) (reg r)) ++ (addq (imm 8) (reg rsp))

(* label function to print int in rdi register *)
let print_int_fun =
        label "print_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab "S_int") (reg rdi) ++
        xorq (reg rax) (reg rax) ++
        call "printf" ++
        ret

(* label function to print float in xmm0 register *)
let print_float_fun =
        label "print_float" ++
        movq (ilab "S_float") (reg rdi) ++
        movq (imm 1) (reg rax) ++
        call "printf" ++
        movq (imm 0) (reg rax) ++
        ret

(* label function to compute factorial *)
let print_fact_fun =
        label "fact_fun" ++
        cmpq (imm 1) (reg rdi) ++
        je "fact_init" ++
        (push_int rdi) ++
        decq (reg rdi) ++
        call "fact_fun" ++
        (pop_int rdi) ++
        imulq (reg rdi) (reg rax) ++
        ret ++
        label "fact_init" ++
        movq (imm 1) (reg rax) ++
        ret

(* label function to compute int power *)
let print_power_int_fun =
        label "power_int_fun" ++
        cmpq (imm 0) (reg rsi) ++
        je "power_int_init" ++
        decq (reg rsi) ++
        call "power_int_fun" ++
        imulq (reg rdi) (reg rax) ++
        ret ++
        label "power_int_init" ++
        movq (imm 1) (reg rax) ++
        ret

(* stack initalisation to prevent segment fault *)
let init_stack = pushq (reg rbp) ++ movq (reg rsp) (reg rbp)
let end_stack = movq (reg rbp) (reg rsp) ++ popq rbp

(* -- code generation -- *)

(* code of operations *)
let code_of_op op =
        match op with
        | ADDI _ -> (pop_int rsi) ++ (pop_int rdi) ++ addq (reg rsi) (reg rdi) ++ (push_int rdi)
        | SUBI _ -> (pop_int rsi) ++ (pop_int rdi) ++ subq (reg rsi) (reg rdi) ++ (push_int rdi)
        | MULI _ -> (pop_int rsi) ++ (pop_int rdi) ++ imulq (reg rsi) (reg rdi) ++ (push_int rdi)
        | DIVI _ -> (pop_int rdi) ++ (pop_int rax) ++ movq (imm 0) (reg rdx) ++ idivq (reg rdi) ++ (push_int rax)
        | MODI _ -> (pop_int rdi) ++ (pop_int rax) ++ movq (imm 0) (reg rdx) ++ idivq (reg rdi) ++ (push_int rdx)
        | NEGI _ -> (pop_int rdi) ++ (negq (reg rdi)) ++ (push_int rdi)
        | ADDF _ -> (pop_float xmm1) ++ (pop_float xmm0) ++ addsd (reg xmm1) (reg xmm0) ++ (push_float xmm0)
        | SUBF _ -> (pop_float xmm1) ++ (pop_float xmm0) ++ subsd (reg xmm1) (reg xmm0) ++ (push_float xmm0)
        | MULF _ -> (pop_float xmm1) ++ (pop_float xmm0) ++ mulsd (reg xmm1) (reg xmm0) ++ (push_float xmm0)
        | NEGF _ -> (pop_float xmm0) ++ xorpd (reg xmm1) (reg xmm1) ++ subsd (reg xmm0) (reg xmm1) ++ (push_float xmm1)
        | INTFUN _ -> (pop_float xmm0) ++ cvttsd2siq (reg xmm0) (reg rdi) ++ (push_int rdi)
        | FLOATFUN _ -> (pop_int rdi) ++ cvtsi2sdq (reg rdi) (reg xmm0) ++ (push_float xmm0)
        | FACT _ -> (pop_int rdi) ++ call "fact_fun" ++ (push_int rax)
        | POWERI _ -> (pop_int rsi) ++ (pop_int rdi) ++ call "power_int_fun" ++ (push_int rax)
        | _ -> failwith "impossible !"

(* convert a tree into code, create float's label numerated as ".LCxxx" *)
let rec code_of_tree tree floatCount floatLabel =
        match tree with
        | INT x ->      (movq (imm (int_of_string x)) (reg rdi) ++ (push_int rdi),
                        floatCount,
                        floatLabel)
        | FLOAT x ->    (movsd (lab (".LC"^(string_of_int floatCount))) (reg xmm0) ++ (push_float xmm0),
                        floatCount + 1,
                        floatLabel ++ label (".LC"^(string_of_int floatCount)) ++ double (float_of_string x))
        | ADDI (t1, t2) | SUBI (t1, t2) | MULI (t1, t2) | DIVI (t1, t2) | MODI (t1, t2) | POWERI (t1, t2)
        | ADDF (t1, t2) | SUBF (t1, t2) | MULF (t1, t2) -> 
                let (subCode1, subFloatCount1, subFloatLabel1) = (code_of_tree t1 floatCount floatLabel) in
                let (subCode2, subFloatCount2, subFloatLabel2) = (code_of_tree t2 subFloatCount1 subFloatLabel1) in
                        (subCode1 ++ subCode2 ++ (code_of_op tree),
                        subFloatCount2, 
                        subFloatLabel2)
        | NEGI t | NEGF t | FLOATFUN t | INTFUN t | FACT t ->
                let (subCode, subFloatCount, subFloatLabel) = (code_of_tree t floatCount floatLabel) in
                        (subCode ++ (code_of_op tree),
                        subFloatCount,
                        subFloatLabel)

(* code of the "print" function depending on the tree type *)
let print_res tree =
        if type_of_tast tree = "int"
                then (pop_int rdi) ++ end_stack ++ call "print_int" ++ ret ++ print_int_fun
                else (pop_float xmm0) ++ end_stack ++ call "print_float" ++ ret ++ print_float_fun

(* ----- Main functions ----- *)

(* convert tast tree into progam type usign previous functions *)
let assembly_of_tree tree =
        let (codeTree, _, floatLabel) = code_of_tree tree 0 nop in
        let code = {text =
                globl "main" ++ label "main" ++
                init_stack ++
                codeTree ++
                (print_res tree) ++
                (print_fact_fun) ++
                (print_power_int_fun);
                data =
                        label "S_int" ++ string "%d \n" ++
                        label "S_float" ++ string "%f \n" ++ 
                        floatLabel} in
        code

(* write a code in a fileName.s file *)
let write_code code fileName =
        let c = open_out (fileName ^ ".s") in
        let fmt = formatter_of_out_channel c in
        X86_64.print_program fmt code;
        close_out c


