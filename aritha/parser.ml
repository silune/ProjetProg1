open Lexer
(* Parser : transform the lexeme list into an AST and then a TAST which are defined as : *)

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


let priorityOrder = [|  [Add_int; Sub_int; Add_float; Sub_float];
                        [Mul_int; Div; Mod; Mul_float];
                        [Int_fun; Float_fun]|]

let priorityMax = Array.length priorityOrder


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

let type_operator lexeme =
        let rec aux i =
                if i >= priorityMax
                        then false
                        else (List.mem lexeme priorityOrder.(i)) || (aux (i + 1))
        in aux 0


(* -- extracts sub lists of lexeme lists -- *)

let next_operation priorityLevel lexemeList =
        let operatorLexemeList = priorityOrder.(priorityLevel) in
        let rec aux lst braCount isUnary =
                match lst with
                | L_bra::q -> let (operator, operand, rest) = aux q (braCount + 1) true in (operator, L_bra::operand, rest)
                | R_bra::q -> let (operator, operand, rest) = aux q (braCount - 1) false in (operator, L_bra::operand, rest)
                | t::q when (t = Add_int || t = Sub_int) && isUnary ->
                                let (operator, operand, rest) = aux q braCount false in (operator, t::operand, rest)
                | t::q when (t = Int_fun || t = Float_fun) && (List.mem t operatorLexemeList) && (braCount = 0) -> (Some t, q, [])
                | t::q when (List.mem t operatorLexemeList) && (braCount = 0) -> (Some t, [], q)
                | t::q when (type_operator t) -> let (operator, operand, rest) = aux q braCount true in (operator, t::operand, rest)
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

(* -- extracts operator / values from lexeme lists -- *)

(* get the values of a list that contains exactly one value (uses Option type) *)
let type_number lexemeList =
        match lexemeList with
        | [Int _] | Add_int::[Int _] | Sub_int::[Int _]
        | [Float _] | Add_int::[Float _] | Sub_int::[Float _] -> true
        | _ -> false

(* -- tree generation -- *)

let number_tree lexemeList =
        match lexemeList with
        | [Int x] | Add_int::[Int x] -> INT x
        | [Float x] | Add_int::[Float x]-> FLOAT x
        | Sub_int::[Int x] -> NEGI (INT x)
        | Sub_int::[Float x] -> NEGF (FLOAT x)
        | _ -> failwith "not a number"

let rec build_tree priorityLevel lexemeList =
        if type_number lexemeList
                then number_tree lexemeList
                else if priorityLevel >= priorityMax
                        then let noBraLexemeList = remove_bra lexemeList in
                                if noBraLexemeList = lexemeList
                                        then failwith "unable to execute expression"
                                        else build_tree 0 noBraLexemeList
                        else let (nextOperator, operand, rest) = next_operation priorityLevel lexemeList in
                                if Option.is_some nextOperator
                                        then run_tree priorityLevel (build_tree (priorityLevel + 1) operand) (Option.get nextOperator) rest
                                        else build_tree (priorityLevel + 1) lexemeList

and run_tree priorityLevel rightTree operator lexemeList =
        let (nextOperator, leftOperand, rest) = next_operation priorityLevel lexemeList in
        let build_left lexList = build_tree priorityLevel lexList in
        let run_with newTree = (if rest = []
                                then newTree
                                else run_tree priorityLevel newTree (Option.get nextOperator) rest) in
        match operator with
        | Add_int -> run_with (ADDI (rightTree, build_left leftOperand))
        | Sub_int -> run_with (SUBI (rightTree, build_left leftOperand))
        | Mul_int -> run_with (MULI (rightTree, build_left leftOperand))
        | Div -> run_with (DIVI (rightTree, build_left leftOperand))
        | Mod -> run_with (MODI (rightTree, build_left leftOperand))
        | Add_float -> run_with (ADDF (rightTree, build_left leftOperand))
        | Sub_float -> run_with (SUBF (rightTree, build_left leftOperand))
        | Mul_float -> run_with (MULF (rightTree, build_left leftOperand))
        | Float_fun -> run_with (FLOATFUN rightTree)
        | Int_fun -> run_with (INTFUN rightTree)
        | _ -> failwith "not implemented yet"
                

(* converts an ast into a tast, verifying if the ast is well typed *)
let verify_type tree =
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
                | INT x -> "int"
                | FLOAT x -> "float"
                | NEGI t -> "int"
                | NEGF t -> "float"
        in ignore (aux tree)

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
        | INTFUN _ | ADDI _ | SUBI _ | MULI _ | DIVI _ | MODI _ | NEGI _ | INT _ -> "int"
        | _ -> "float"






