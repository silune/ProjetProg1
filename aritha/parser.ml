open Lexer;;

type exp =
        | INTFUN of exp
        | FLOATFUN of exp
        | ADDI of exp * exp
        | SUBI of exp * exp
        | MULI of exp * exp
        | DIVI of exp * exp
        | MODI of exp * exp
        | ADDF of exp * exp
        | SUBF of exp * exp
        | MULF of exp * exp
        | NEG of exp (* not typed in ast (will be in tast) *)
        | INT of string
        | FLOAT of string

let verify_function lexemeList = 
        let rec aux lst funCount braCount =
                match lst with
                | Int_fun::L_bra::q -> aux q (funCount + 1) (braCount + 1)
                | Float_fun::L_bra::q -> aux q (funCount + 1) (braCount + 1)
                | Int_fun::q -> failwith "'int' function need partenthesis"
                | Float_fun::q -> failwith "'float' function need parenthesis"
                | L_bra::q when funCount <> 0 -> aux q funCount (braCount + 1)
                | R_bra::q when funCount <> 0 -> if funCount = braCount
                                                        then aux q (funCount - 1) (braCount - 1)
                                                        else aux q funCount (braCount - 1)
                | t::q -> aux q funCount braCount
                | [] when funCount = 0 -> ()
                | [] -> failwith "unmatched parenthesis on some function"
        in aux lexemeList 0 0;;

let verify_parenthesis lexemeList =
        let rec aux lst braCount =
                if braCount < 0
                        then failwith "unmatched parenthesis"
                        else match lst with
                        | L_bra::q -> aux q (braCount + 1)
                        | R_bra::q -> aux q (braCount - 1)
                        | t::q -> aux q braCount
                        | [] when braCount = 0 -> ()
                        | [] -> failwith "unmatched ')' parenthesis"
        in aux lexemeList 0;;

let operand_of_operator operatorLexeme lexemeList =
        let rec aux lst braCount =
                match lst with
                | L_bra::q -> (match aux q (braCount + 1) with
                                | [] -> failwith "cette situation n'arrive jamais mais sinon ocaml warning :)"
                                | tRes::qRes -> (L_bra::tRes)::qRes)
                | R_bra::q -> (match aux q (braCount - 1) with
                                | [] -> failwith "pareil"
                                | tRes::qRes -> (R_bra::tRes)::qRes)
                | t::q when (t = operatorLexeme) && (braCount = 0) -> [[]; q]
                | t::q -> (match aux q braCount with
                                | [] -> failwith "encore"
                                | tRes::qRes -> (t::tRes)::qRes)
                | [] -> [[]]
        in aux lexemeList 0;;

let next_operator operatorLexemeList lexemeList =
        let rec aux lst parentCount =
                match lst with
                | L_bra::q -> aux q (parentCount + 1)
                | R_bra::q -> aux q (parentCount - 1)
                | t::q when (List.mem t operatorLexemeList) && (parentCount = 0) -> Some t
                | t::q -> aux q parentCount
                | [] -> None
        in aux lexemeList 0;;

let remove_bra lexemeList =
        if (List.hd lexemeList <> L_bra)
                then failwith "unreadable expression"
                else let rec aux lst =
                        match lst with
                        | [] -> failwith "unreadable expression"
                        | R_bra::[] -> []
                        | t::q -> t::(aux q)
                in aux (List.tl lexemeList);;
        
let operand_of_function lexemeList =
        if lexemeList = []
                then failwith "unreadable expression"
                else remove_bra (List.tl lexemeList);;

let type_number lexemeList =
        match lexemeList with
        | [Int x] -> Some (Int x)
        | [Float x] -> Some (Float x)
        | _ -> None;;

let rec parenthise_minus lexemeList =
        match lexemeList with
        | t1::Sub_int::t2::q when (Option.is_some (type_number [t1])) || (t1 = L_bra) -> t1::Sub_int::(parenthise_minus (t2::q))
        | t1::Sub_int::t2::q -> t1::L_bra::Sub_int::t2::R_bra::(parenthise_minus q)
        | Sub_int::t::q when (Option.is_some (type_number [t])) -> L_bra::Sub_int::t::R_bra::(parenthise_minus q)
        | t::q -> t::(parenthise_minus q)
        | [] -> [];;

let rec tree_of node arguments =
        match node, arguments with
        | Int_fun, [lexList] -> INTFUN (aux_ast lexList)
        | Int_fun, _ -> failwith "'int' takes exactly one argument"
        | Float_fun, [lexList] -> FLOATFUN (aux_ast lexList)
        | Float_fun, _ -> failwith "'float' takes exactly one argument"
        | Add_int, []::[lexList] -> aux_ast lexList
        | Add_int, lexList1::[lexList2] -> ADDI (aux_ast lexList1, aux_ast lexList2)
        | Add_int, _ -> failwith "'+' takes one (on left) or two argument"
        | Sub_int, []::[lexList] -> NEG (aux_ast lexList)
        | Sub_int, lexList1::[lexList2] -> SUBI (aux_ast lexList1, aux_ast lexList2)
        | Sub_int, _ -> failwith "'-' takes one (on left) or two argument"
        | Mul_int, lexList1::[lexList2] -> MULI (aux_ast lexList1, aux_ast lexList2)
        | Mul_int, _ -> failwith "'*' takes exactly two arguments"
        | Div, lexList1::[lexList2] -> DIVI (aux_ast lexList1, aux_ast lexList2)
        | Div, _ -> failwith "'/' takes exaclty two arguments"
        | Mod, lexList1::[lexList2] -> MODI (aux_ast lexList1, aux_ast lexList2)
        | Mod, _ -> failwith "'%' takes exactly two arguments"
        | Add_float, lexList1::[lexList2] -> ADDF (aux_ast lexList1, aux_ast lexList2)
        | Add_float, _ -> failwith "'+.' takes exaclty two arguments"
        | Sub_float, lexList1::[lexList2] -> SUBF (aux_ast lexList1, aux_ast lexList2)
        | Sub_float, _ -> failwith "'-.' takes exaclty two arguments"
        | Mul_float, lexList1::[lexList2] -> MULF (aux_ast lexList1, aux_ast lexList2)
        | Mul_float, _ -> failwith "'*.' takes exaclty two arguments"
        | Float x, _ -> FLOAT x
        | Int x, _ -> INT x
        | _, _ -> failwith "unknown operator encountered"

and aux_ast lexemeList =
        let number = type_number lexemeList in
        if Option.is_some number
                then tree_of (Option.get number) [lexemeList]
                else let nextOp1 = next_operator [Add_int; Sub_int; Add_float; Sub_float] lexemeList in
                if Option.is_some nextOp1
                        then let op = (Option.get nextOp1) in tree_of op (operand_of_operator op lexemeList)
                        else let nextOp2 = next_operator [Mul_int; Div; Mod; Mul_float] lexemeList in
                        if Option.is_some nextOp2
                                then let op = (Option.get nextOp2) in tree_of op (operand_of_operator op lexemeList)
                                else let nextOp3 = next_operator [Int_fun; Float_fun] lexemeList in
                                if Option.is_some nextOp3
                                        then let op = (Option.get nextOp3) in tree_of op [operand_of_function lexemeList]
                                        else let nextLexList = remove_bra lexemeList in
                                        if nextLexList = lexemeList
                                                then failwith "unable to execute expression"
                                                else aux_ast nextLexList;;

let syntax_analyser lexemeList =
        verify_function lexemeList;
        verify_parenthesis lexemeList;
        aux_ast (parenthise_minus lexemeList);;




