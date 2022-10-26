open Lexer
(* Parser : transform the lexeme list into an AST and then a TAST which are defined as : *)

(* AST type definition *)
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
        | NEG of ast (* not typed in ast *)
        | INT of string
        | FLOAT of string

(* TAST type definition *)
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

(* ----- Auxilary functions ----- *)

(* -- syntax verification functions -- *)

(* verifies if all parenthesis are matching in a lexeme list *)
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
        in aux lexemeList 0

(* verifies if functions are well called using parenthesis *)
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
        in aux lexemeList 0 0

(* -- extracts sub lists of lexeme lists -- *)

(* get the right and left parts of a list splited on the first operator met *)
let operand_of_operator operatorLexeme lexemeList =
        let rec aux lst braCount =
                match lst with
                (* nb : by construction (List.hd res) is defined because res cannot be empty *)
                | L_bra::q -> let res = aux q (braCount + 1) in (L_bra::(List.hd res))::(List.tl res)
                | R_bra::q -> let res = aux q (braCount - 1) in (R_bra::(List.hd res))::(List.tl res)
                | t::q when (t = operatorLexeme) && (braCount = 0) -> [[]; q]
                | t::q -> let res = aux q braCount in (t::(List.hd res))::(List.tl res)
                | [] -> [[]]
        in aux lexemeList 0

(* get the lexemes inside parenthesis, fail if the first and last elements are not parenthesis *)
let remove_bra lexemeList =
        if (List.hd lexemeList <> L_bra)
                then failwith "unreadable expression"
                else let rec aux lst =
                        match lst with
                        | [] -> failwith "unreadable expression"
                        | R_bra::[] -> []
                        | t::q -> t::(aux q)
                in aux (List.tl lexemeList)

(* extracts operand of a function, assuming first argument is a function lexeme *)
let operand_of_function lexemeList =
        if lexemeList = []
                then failwith "unreadable expression"
                else remove_bra (List.tl lexemeList)

(* -- extracts operator / values from lexeme lists -- *)

(* get the values of a list that contains exactly one value (uses Option type) *)
let type_number lexemeList =
        match lexemeList with
        | [Int x] -> Some (Int x)
        | [Float x] -> Some (Float x)
        | _ -> None

(* get the first operator met in the list which is in the operator searched list (at zero level of parenthesis) *)
let next_operator operatorLexemeList lexemeList =
        let rec aux lst braCount =
                match lst with
                | L_bra::q -> aux q (braCount + 1)
                | R_bra::q -> aux q (braCount - 1)
                | t::q when (List.mem t operatorLexemeList) && (braCount = 0) -> Some t
                | t::q -> aux q braCount
                | [] -> None
        in aux lexemeList 0

(* -- tree generation -- *)

(* converts a lexeme and a list of sub lists into the associated node in the tree (ast) *)
let rec tree_of node arguments =
        ((match node, arguments with
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
                                        else if (Option.is_some (type_number [t]))
                                                then aux_ast (L_bra::Sub_int::t::R_bra::q)
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
        | _, _ -> failwith "unknown operator encountered") : ast)

(* converts a lexeme list in a tree (ast) by searching the next operation (in priority order) and recursivly building the nodes *)
and aux_ast lexemeList =
        let number = type_number lexemeList in
        if Option.is_some number (* if lexemeList is just a number value *)
                then tree_of (Option.get number) [lexemeList]
                else let nextOp1 = next_operator [Add_int; Sub_int; Add_float; Sub_float] lexemeList in
                if Option.is_some nextOp1 (* if nextOperator is +, -, +. or -. *)
                        then let op = (Option.get nextOp1) in tree_of op (operand_of_operator op lexemeList)
                        else let nextOp2 = next_operator [Mul_int; Div; Mod; Mul_float] lexemeList in
                        if Option.is_some nextOp2 (* if nextOperator is *, /, %, *. *)
                                then let op = (Option.get nextOp2) in tree_of op (operand_of_operator op lexemeList)
                                else let nextOp3 = next_operator [Int_fun; Float_fun] lexemeList in
                                if Option.is_some nextOp3 (* if there is no nextOperator but a function int or float *)
                                        then let op = (Option.get nextOp3) in tree_of op [operand_of_function lexemeList]
                                        else let noBraLexList = remove_bra lexemeList in 
                                        if noBraLexList = lexemeList (* if there is no operator or function, test parenthesis *)
                                                then failwith "unable to execute expression"
                                                else aux_ast noBraLexList

(* generate the ast from a lexeme list, verifies parenthesis are matching and functions are well wrote *)
let untyped_syntax_analyser lexemeList =
        verify_function lexemeList;
        verify_parenthesis lexemeList;
        aux_ast lexemeList

(* converts an ast into a tast, verifying if the ast is well typed *)
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
        in fst (aux tree)

(* ----- Main functions ----- *)

(* converts a lexeme list into a tast using previous functions *)
let syntax_analyser lexemeList = tast_of_ast (untyped_syntax_analyser lexemeList)

(* gives the type of a tast (using strings "int" and "float") *)
let rec type_of_tast tree =
        match tree with
        | INTFUN _ | ADDI _ | SUBI _ | MULI _ | DIVI _ | MODI _ | NEGI _ | INT _ -> "int"
        | _ -> "float"






