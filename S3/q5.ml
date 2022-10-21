type lexeme = 
        | Start
        | End
        | Det
        | Sep
        | SepFinal
        | Argum
        | Var of string
        | AddOp
        | SousOp
        | MultOp

(*Analyseur lexical qui transforme text -> lexemes*)

(*text en liste des mots / virgules*)
let sep_text text =
        let imax = String.length text in
        let rec aux i mot =
                if i >= imax
                        then if mot = "" then [] else [mot]
                        else match text.[i] with
                        | ' ' -> if (String.length mot) > 0
                                        then mot::(aux (i+1) "")
                                        else aux (i+1) ""
                        | ',' -> mot::","::(aux (i+1) "")
                        | _ -> aux (i+1) (mot^(String.make 1 text.[i]))
        in aux 0 "";;

(*verifie si un mot est un nombre (entier ou flottant)*)
let is_number str =
        let imax = String.length str in
        let rec aux i =
                if i >= imax
                        then true
                        else (('0' <= str.[i]) && (str.[i] <= '9')) || (str.[i] = '.')
                                && (aux (i+1))
        in aux 0;;

(*transforme un texte en liste de lexemes*)
let analyse_lex text =
        let rec aux lst =
                match lst with
                | "Fais"::q -> Start::(aux q)
                | "fais"::q -> Start::(aux q)
                | "stp"::q -> End::(aux q)
                | "la"::q -> Det::(aux q)
                | ","::q -> Sep::(aux q)
                | "et"::q -> SepFinal::(aux q)
                | "de"::q -> Argum::(aux q)
                | "somme"::q -> AddOp::(aux q)
                | "soustraction"::q -> SousOp::(aux q)
                | "multiplication"::q -> MultOp::(aux q)
                | t::q when (is_number t) -> (Var t)::(aux q)
                | t::q -> failwith (t^" is not defined")
                | [] -> []
        in aux (sep_text text);;

(*Analyseur Syntaxique*)

type abs =
        | Empty
        | Node of lexeme * abs * abs

(*Retourne la liste des arguments d'une fonction*)
let list_argu lexlist =
        let rec aux l count fin =
                if l = []
                        then if count = 0
                                then [[]]
                                else failwith "erreur gestion , / et"
                        else match (List.hd l), count with
                        | x, _ when fin -> let res = aux (List.tl l) 0 true in
                                        if res = []
                                                then failwith "erreur gestion , / et"
                                                else (x::(List.hd res))::(List.tl res)
                        | Sep, 0 -> aux (List.tl l) 0 fin
                        | SepFinal, 0 -> aux (List.tl l) 0 true
                        | Var x, 0 -> [Var x]::(aux (List.tl l) 0 fin)
                        | Det, i -> let res = aux (List.tl l) (i+1) fin in
                                        if res = []
                                                then failwith "erreur gestion , / et"
                                                else (Det::(List.hd res))::(List.tl res)
                        | SepFinal, i -> let res = aux (List.tl l) (i-1) false in
                                        if res = []
                                                then failwith "erreur gestion , / et"
                                                else (SepFinal::(List.hd res))::(List.tl res)
                        | x, i -> let res = aux (List.tl l) i fin in 
                                        if res = []
                                                then failwith "erreur gestion , / et"
                                                else (x::(List.hd res))::(List.tl res)
        in aux lexlist 0 false;;

let rec check_end lexlist =
        match lexlist with
        | t::q when t = End -> []
        | t::q -> t::(check_end q)
        | [] -> failwith "manque de politesse";;

let check_start lexlist =
        if (List.tl lexlist = [])
                then failwith "pas d'ordre donné"
                else if (List.hd lexlist = Start)
                        then (List.tl lexlist)
                        else failwith "donner un ordre";;

let string_of_op op =
        match op with
        | AddOp -> "somme"
        | MultOp -> "multiplication"
        | SousOp -> "soustraction"
        | _ -> failwith "operation invalide";;

let rec aux_tree lst =
        match lst with
        | Det::AddOp::Argum::q -> tree_of AddOp (list_argu q)
        | Det::MultOp::Argum::q -> tree_of MultOp (list_argu q)
        | Det::SousOp::Argum::q -> tree_of SousOp (list_argu q)
        | [Var x] -> Node (Var x, Empty, Empty)
        | t::q -> failwith "t::q"
        | [] -> failwith "[]";;

let rec tree_of op lstargu =
        match lstargu with
        | [] -> failwith "opération "^(string_of_op op)^" nécessite des arguments"
        | [x] -> aux_tree x
        | t::q -> Node (op, (aux_tree t), tree_of op lstargu);;


let analyse_syntaxique lexlist =
        aux_tree (check_start (check_end lexlist));;











