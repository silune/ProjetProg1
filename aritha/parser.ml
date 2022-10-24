open Lexer;;

type ast =
        | INTFUN of ast
        | FLOATFUN of ast
        | ADDI of ast * ast
        | SUBI of ast * ast
        | MULI of ast * ast
        | DIVI of ast * ast
        | MODI of ast * ast
        | ADDF of ast * ast
        | SUBF of ast * ast
        | MULF of ast * ast
        | NEG of ast (* not typed in ast (will be in tast) *)
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

let rec tree_of node arguments =
        match node, arguments with
        | Int_fun, [lexList] -> INTFUN (aux_ast lexList)
        | Int_fun, _ -> failwith "'int' takes exactly one argument"
        | Float_fun, [lexList] -> FLOATFUN (aux_ast lexList)
        | Float_fun, _ -> failwith "'float' takes exactly one argument"
        | Add_int, []::[t::q] -> if t = L_bra || (Option.is_some (type_number [t]))
                                        then aux_ast (t::q)
                                        else failwith "plus sign expected parenthesis"
        | Add_int, lexList1::[lexList2] -> ADDI (aux_ast lexList1, aux_ast lexList2)
        | Add_int, _ -> failwith "'+' takes one (on left) or two argument"
        | Sub_int, []::[t::q] -> if t = L_bra || (Option.is_some (type_number [t]))
                                        then NEG (aux_ast (t::q))
                                        else failwith "negation expected parenthesis"
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

let untyped_syntax_analyser lexemeList =
        verify_function lexemeList;
        verify_parenthesis lexemeList;
        aux_ast lexemeList;;

type tast =
        | INTFUN of tast
        | FLOATFUN of tast
        | ADDI of tast * tast
        | SUBI of tast * tast
        | MULI of tast * tast
        | DIVI of tast * tast
        | MODI of tast * tast
        | ADDF of tast * tast
        | SUBF of tast * tast
        | MULF of tast * tast
        | NEGI of tast
        | NEGF of tast
        | INT of string
        | FLOAT of string


let tast_of_ast tree =
        let rec aux (treeRec : ast) =
                match treeRec with
                | INTFUN t -> let (f, fType) = aux t in if fType = "float"
                                                        then (INTFUN f, "int")
                                                        else failwith "float type expected in 'int' function"
                | FLOATFUN t -> let (f, fType) = aux t in if fType = "int"
                                                        then (FLOATFUN f, "float")
                                                        else failwith "int type expected in 'int' function" 
                | ADDI (t1, t2) -> let ((f1, f1Type), (f2, f2Type)) = (aux t1, aux t2) in if (f1Type = "int") && (f2Type = "int")
                                                        then (ADDI (f1, f2), "int")
                                                        else failwith "int type expected with '+' operator" 
                | SUBI (t1, t2) -> let ((f1, f1Type), (f2, f2Type)) = (aux t1, aux t2) in if (f1Type = "int") && (f2Type = "int")
                                                        then (SUBI (f1, f2), "int")
                                                        else failwith "int type expected with '-' operator" 
                | MULI (t1, t2) -> let ((f1, f1Type), (f2, f2Type)) = (aux t1, aux t2) in if (f1Type = "int") && (f2Type = "int")
                                                        then (MULI (f1, f2), "int")
                                                        else failwith "int type expected with '*' operator" 
                | DIVI (t1, t2) -> let ((f1, f1Type), (f2, f2Type)) = (aux t1, aux t2) in if (f1Type = "int") && (f2Type = "int")
                                                        then (DIVI (f1, f2), "int")
                                                        else failwith "int type expected with '/' operator" 
                | MODI (t1, t2) -> let ((f1, f1Type), (f2, f2Type)) = (aux t1, aux t2) in if (f1Type = "int") && (f2Type = "int")
                                                        then (MODI (f1, f2), "int")
                                                        else failwith "int type expected with '%' operator" 
                | ADDF (t1, t2) -> let ((f1, f1Type), (f2, f2Type)) = (aux t1, aux t2) in if (f1Type = "float") && (f2Type = "float")
                                                        then (ADDF (f1, f2), "float")
                                                        else failwith "float type expected with '+.' operator" 
                | SUBF (t1, t2) -> let ((f1, f1Type), (f2, f2Type)) = (aux t1, aux t2) in if (f1Type = "float") && (f2Type = "float")
                                                        then (SUBF (f1, f2), "float")
                                                        else failwith "float type expected with '-.' operator" 
                | MULF (t1, t2) -> let ((f1, f1Type), (f2, f2Type)) = (aux t1, aux t2) in if (f1Type = "float") && (f2Type = "float")
                                                        then (MULF (f1, f2), "float")
                                                        else failwith "float type expected with '*.' operator"
                | NEG t -> let (f, fType) = aux t in if fType = "int"
                                                        then (NEGI f, "int")
                                                        else (NEGF f, "float")
                | INT x -> (INT x, "int")
                | FLOAT x -> (FLOAT x, "float")
        in fst (aux tree);;

let syntax_analyser lexemeList = tast_of_ast (untyped_syntax_analyser lexemeList);;

let rec type_of_tast tree =
        match tree with
        | INTFUN _ | ADDI _ | SUBI _ | MULI _ | DIVI _ | MODI _ | NEGI _ | INT _ -> "int"
        | _ -> "float";;


