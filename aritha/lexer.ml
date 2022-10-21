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
        let imax = (String.length str) in
        let rec aux i =
                if i >= imax then true
                else if (('0' <= str.[i]) && (str.[i] <= '9')) || (str.[i] = '.')
                        then aux (i+1)
                        else false
        in aux 0;;

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
        | _ -> if is_float str
                then Some (Float(str))
                else if is_int str
                        then Some (Int(str))
                        else None;;
let string_of_char c =
        (String.make 1 c);;

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
                                else failwith ("\""^buffer^"\" is not recognized")
                        else let nextChar = (string_of_char stringExpS.[i]) in
                                let nextBuffer = (buffer ^ nextChar) in
                                if (Option.is_some (string_to_lexeme nextBuffer))
                                        then aux (i+1) nextBuffer
                                        else (Option.get bufferLex)::(aux (i+1) nextChar)
        in if imax = 0
                then failwith "empty file"
                else aux 1 (string_of_char (stringExpS.[0]));;



