type lexeme =
        | L_bra
        | R_bra
        | Int_fun
        | Float_fun
        | Add_int
        | Sub_int
        | Mul_int
        | Div
        | Mod
        | Add_float
        | Sub_float
        | Mul_float
        | Int of string
        | Float of string


(* Verifie si une chaine de caractères est du format d'un entier *)
let is_int str =
        let imax = (String.length str) in
        let rec aux i =
                if i >= imax then true
                else if ('0' <= str.[i]) && (str.[i] <= '9')
                        then aux (i+1)
                        else false
        in aux 0;;

(* Verifie si une chaine de caractères est du format d'un flottant *)
let is_float str =
        if str = "."
                then false
                else let splited_str = (String.split_on_char '.' str) in
                        match splited_str with
                        | a::b::[] -> (is_int a) && (is_int b)
                        | _ -> false;;

(* Convertit une chaine de caractères en une option de lexeme si elle est reconnue, en None sinon *)
let string_to_lexeme str =
        match str with
        | "(" -> Some L_bra
        | ")" -> Some R_bra
        | "int" -> Some Int_fun
        | "float" -> Some Float_fun
        | "+" -> Some Add_int
        | "-" -> Some Sub_int
        | "*" -> Some Mul_int
        | "/" -> Some Div
        | "%" -> Some Mod
        | "+." -> Some Add_float
        | "_." -> Some Sub_float
        | "*." -> Some Mul_float
        | _ -> if is_int str
                then Some (Int(str))
                else if is_float str
                        then Some (Float(str))
                        else None;;
let string_of_char c =
        (String.make 1 c);;

let is_a_lexeme_prefix str =
        if (is_float str) || (is_int str)
                then true
                else let lexeme_patterns = ["("; ")"; "int"; "float"; "+"; "-"; "*"; "+."; "-."; "*."] in
                        let rec aux lst =
                                match lst with
                                | [] -> false
                                | t::q when (String.starts_with ~prefix:str t) -> true
                                | t::q -> aux q
                        in aux lexeme_patterns;;

let skip str skip_cara =
        let imax = (String.length str) in
        let rec aux i =
                if i >= imax then ""
                else if (List.mem str.[i] skip_cara)
                        then aux (i+1)
                        else (string_of_char str.[i])^(aux (i+1))
        in aux 0;;

let lexical_analyser stringExp =
        let stringExpS = (skip stringExp [' ']) in
        let imax = (String.length stringExpS) in
        let rec aux i buffer =
                let bufferLex = string_to_lexeme buffer in
                if i >= imax
                        then if (Option.is_some bufferLex)
                                then [Option.get bufferLex]
                                else failwith ("'"^buffer^"' is not recognized")
                        else let nextChar = (string_of_char stringExpS.[i]) in
                                let nextBuffer = (buffer ^ nextChar) in
                                if (is_a_lexeme_prefix nextBuffer)
                                        then aux (i+1) nextBuffer
                                        else if (Option.is_some bufferLex)
                                                then (Option.get bufferLex)::(aux (i+1) nextChar)
                                                else failwith ("'"^buffer^"' is not recognized")
        in if imax = 0
                then failwith "empty file"
                else aux 1 (string_of_char (stringExpS.[0]));;



