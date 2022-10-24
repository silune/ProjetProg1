open Format
open X86_64
open Parser


let push_int r = pushq (reg r);;
let pop_int r = popq r;;

let rec code_of tree =
        let code t1 t2 = (code_of t1) ++ (code_of t2) in
        match tree with
        | INT x -> movq (imm (int_of_string x)) (reg rdi) ++ (push_int rdi)
        | ADDI (t1, t2) -> (code t1 t2) ++ (pop_int rdi) ++ (pop_int rsi) ++ (addq (reg rdi) (reg rsi)) ++ (push_int rsi)
        | SUBI (t1, t2) -> (code t1 t2) ++ (pop_int rdi) ++ (pop_int rsi) ++ (subq (reg rdi) (reg rsi)) ++ (push_int rsi)
        | MULI (t1, t2) -> (code t1 t2) ++ (pop_int rdi) ++ (pop_int rsi) ++ (imulq (reg rdi) (reg rsi)) ++ (push_int rsi)
        | DIVI (t1, t2) -> (code t1 t2) ++ (pop_int rax) ++ (pop_int rdi) ++ (movq (imm 0) (reg rdx)) ++ (idivq (reg rdi)) ++ (push_int rax)
        | MODI (t1, t2) -> (code t1 t2) ++ (pop_int rax) ++ (pop_int rdi) ++ (movq (imm 0) (reg rdx)) ++ (idivq (reg rdi)) ++ (push_int rdx)
        | NEGI t -> (code_of t) ++ (pop_int rdi) ++ (negq (reg rdi)) ++ (push_int rdi)
        | _ -> failwith "not implemented in assembly yet";;

let print_int_fun =
        label "print_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab "S_int") (reg rdi) ++
        xorq (reg rax) (reg rax) ++
        call "printf" ++
        ret

let assembly_of_tree tree =
        let code = {text =
                globl "main" ++ label "main" ++
                (code_of tree) ++
                (pop_int rdi) ++
                call "print_int" ++
                ret ++
                print_int_fun;
                data =
                        label "S_int" ++ string "%d";} in
        let c = open_out "test.s" in
        let fmt = formatter_of_out_channel c in
        X86_64.print_program fmt code;
        close_out c;;

assembly_of_tree (ADDI (INT "3", SUBI (INT "5", MULI (NEGI (INT "2"), DIVI (INT "4", MODI (INT "10", INT "7"))))));;
