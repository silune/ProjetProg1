open Lexer
(* Parser : transform the lexeme list into an TAST (verfying some syntax construction and respecting priority order : *)

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
        | DIVF of tast * tast
        | NEGI of tast
        | NEGF of tast
        | FACT of tast
        | POWER of tast * tast
        | INT of string
        | FLOAT of string

(* Priority Order definition *)
let priorityOrder = [|  [Add_int; Sub_int; Add_float; Sub_float];
                        [Mul_int; Div_int; Mod; Mul_float; Div_float];
                        [Fact; Power]|]

let priorityMax = Array.length priorityOrder


(* ----- Auxilary functions ----- *)

(* -- get informations from lexeme lists -- *)

(* says if a list of lexemes is just a number *)
let type_number lexemeList =
        match lexemeList with
        | [Int _] | Add_int::[Int _] | Sub_int::[Int _]
        | [Float _] | Add_int::[Float _] | Sub_int::[Float _] -> true
        | _ -> false

(* says if a lexeme is an operator  *)
let type_operator lexeme =
        let rec aux i =
                if i >= priorityMax
                        then false
                        else (List.mem lexeme priorityOrder.(i)) || (aux (i + 1))
        in aux 0

let type_function lexeme =
        match lexeme with
        | Int_fun | Float_fun | Add_int | Sub_int -> true
        | _ -> false

(* gives the type of any tree, fail if the tree is not well typed *)
let get_type tree =
        let rec aux treeRec =
                match treeRec with
                | INTFUN t -> if aux t = "float" then "int" else failwith "float type expected in 'int' function"
                | FLOATFUN t -> if aux t = "int" then "float" else failwith "int type expected in 'int' function" 
                | ADDI (t1, t2) -> if (aux t1 = "int") && (aux t2 = "int") then "int" else failwith "int type expected with '+' operator" 
                | SUBI (t1, t2) -> if (aux t1 = "int") && (aux t2 = "int") then "int" else failwith "int type expected with '-' operator" 
                | MULI (t1, t2) -> if (aux t1 = "int") && (aux t2 = "int") then "int" else failwith "int type expected with '*' operator" 
                | DIVI (t1, t2) -> if (aux t1 = "int") && (aux t2 = "int") then "int" else failwith "int type expected with '/' operator" 
                | MODI (t1, t2) -> if (aux t1 = "int") && (aux t2 = "int") then "int" else failwith "int type expected with '%' operator"
                | ADDF (t1, t2) -> if (aux t1 = "float") && (aux t2 = "float") then "float" else failwith "float type expected with '+.' operator" 
                | SUBF (t1, t2) -> if (aux t1 = "float") && (aux t2 = "float") then "float" else failwith "float type expected with '-.' operator" 
                | MULF (t1, t2) -> if (aux t1 = "float") && (aux t2 = "float") then "float" else failwith "float type expected with '*.' operator"
                | DIVF (t1, t2) -> if (aux t1 = "float") && (aux t2 = "float") then "float" else failwith "float type expected with '/.' operator"
                | FACT t -> if (aux t = "int") then "int" else failwith "int type expected with '!' operator"
                | POWER (t1, t2) -> if (aux t1 = "int") && (aux t2 = "int") then "int" else failwith "int type expected with '^' operator"
                | INT x -> "int"
                | FLOAT x -> "float"
                | NEGI t -> "int"
                | NEGF t -> "float"
        in aux tree

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

(* verifies if a tree is well typed  *)
let verify_type tree = ignore (get_type tree)

(* -- extracts sub lists of lexeme lists -- *)

(* get the triplet (see below) assuming le list is just a function *)
let get_function_operation lexemeList =
        match lexemeList with
        | [] -> (None, [], [])
        | t::q when type_function t -> (Some t, q, [])
        | t::q -> (None, lexemeList, [])

(* get the triplet (next operator, next operand, rest of the list) of the lexeme list
        - next operator is the first (from left to right) that respect priority level and bracket level
        - next operand is the lexeme list of - the operand if unary operator, - the left operand if binary operator (for assiociativity)
        - rest of the list is all the list of all orther lexemes, some might be ignored if the expression is not valid *)
let next_operation priorityLevel lexemeList =
        if priorityLevel >= priorityMax
                then get_function_operation lexemeList
                else let operatorLexemeList = priorityOrder.(priorityLevel) in
                        let rec aux lst braCount isUnary =
                                match lst with
                                | L_bra::q -> let (operator, operand, rest) = aux q (braCount + 1) true in (operator, L_bra::operand, rest)
                                | R_bra::q -> let (operator, operand, rest) = aux q (braCount - 1) false in (operator, R_bra::operand, rest)
                                | t::q when (t = Add_int || t = Sub_int) && isUnary -> 
                                                let (operator, operand, rest) = aux q braCount false in (operator, t::operand, rest)
                                | t::q when (List.mem t operatorLexemeList) && (braCount = 0) -> (Some t, [], q)
                                | t::q when (type_operator t) && (t <> Fact) ->
                                                let (operator, operand, rest) = aux q braCount true in (operator, t::operand, rest)
                                | t::q -> let (operator, operand, rest) = aux q braCount false in (operator, t::operand, rest)
                                | _ -> (None, [], [])
                        in aux lexemeList 0 true


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

(* -- tree generation -- *)

(* generates the base tree of numbers *)
let number_tree lexemeList =
        match lexemeList with
        | [Int x] | Add_int::[Int x] -> INT x
        | [Float x] | Add_int::[Float x]-> FLOAT x
        | Sub_int::[Int x] -> NEGI (INT x)
        | Sub_int::[Float x] -> NEGF (FLOAT x)
        | _ -> failwith "not a number"

(* generates the tree from a list and a priority level *)
let rec build_tree priorityLevel lexemeList =
        if type_number lexemeList
                then number_tree lexemeList
                else if priorityLevel > priorityMax
                        then let noBraLexemeList = remove_bra lexemeList in
                                if noBraLexemeList = lexemeList
                                        then failwith "unable to execute expression"
                                        else build_tree 0 noBraLexemeList
                        else let (nextOperator, operand, rest) = next_operation priorityLevel lexemeList in
                                if Option.is_some nextOperator
                                        then run_tree priorityLevel (build_tree (priorityLevel + 1) operand) (Option.get nextOperator) rest
                                        else build_tree (priorityLevel + 1) lexemeList

(* concatenate the tree of the left operand with the tree generated by the rest of the list *)
and run_tree priorityLevel leftTree operator lexemeList =
        let (nextOperator, rightOperand, nextRest) = next_operation priorityLevel lexemeList in
        let build_right lexList = build_tree priorityLevel lexList in
        let run_with newTree = (if nextRest = []
                                then newTree
                                else run_tree priorityLevel newTree (Option.get nextOperator) nextRest) in
        match operator with
        | Add_int when priorityLevel = priorityMax -> run_with leftTree
        | Add_int -> run_with (ADDI (leftTree, build_right rightOperand))
        | Sub_int when priorityLevel = priorityMax -> if (get_type leftTree) = "int"
                                                                then run_with (NEGI (leftTree))
                                                                else run_with (NEGF (leftTree))
        | Sub_int -> run_with (SUBI (leftTree, build_right rightOperand))
        | Mul_int -> run_with (MULI (leftTree, build_right rightOperand))
        | Div_int -> run_with (DIVI (leftTree, build_right rightOperand))
        | Mod -> run_with (MODI (leftTree, build_right rightOperand))
        | Add_float -> run_with (ADDF (leftTree, build_right rightOperand))
        | Sub_float -> run_with (SUBF (leftTree, build_right rightOperand))
        | Mul_float -> run_with (MULF (leftTree, build_right rightOperand))
        | Div_float -> run_with (DIVF (leftTree, build_right rightOperand))
        | Float_fun -> run_with (FLOATFUN leftTree)
        | Int_fun -> run_with (INTFUN leftTree)
        | Fact -> run_with (FACT leftTree)
        | Power -> run_with (POWER (leftTree, build_right rightOperand))
        | _ -> failwith "not implemented yet"
                
(* ----- Main functions ----- *)

(* converts a lexeme list into a tast using previous functions *)
let syntax_analyser lexemeList = 
        verify_function lexemeList;
        verify_parenthesis lexemeList;
        let res = build_tree 0 lexemeList in
        verify_type res; res

(* gives the type of a tast (using strings "int" and "float") *)
let rec type_of_tast tree =
        match tree with
        | INTFUN _ | ADDI _ | SUBI _ | MULI _ | DIVI _ | MODI _ | NEGI _ | INT _ | FACT _ | POWER _ -> "int"
        | _ -> "float"

