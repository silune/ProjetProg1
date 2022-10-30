(* Lexer : transform string of .exp file in a lexeme list, lexemes are define as : *)

(* Lexeme type definition*)
type lexeme =
        | L_bra
        | R_bra
        | Int_fun
        | Float_fun
        | Add_int
        | Sub_int
        | Mul_int
        | Div_int
        | Mod
        | Add_float
        | Sub_float
        | Mul_float
        | Div_float
        | Fact
        | Power
        | Int of string
        | Float of string

(* ----- Auxilary functions ----- *)

(* -- String managment functions -- *)

(* converts a char into a string *)
let string_of_char c =
        (String.make 1 c);;

(* skips some characters in a given string *)
let skip str skip_cara =
        let imax = (String.length str) in
        let rec aux i =
                if i >= imax then ""
                else if (List.mem str.[i] skip_cara)
                        then aux (i+1)
                        else (string_of_char str.[i])^(aux (i+1))
        in aux 0;;

(* checks if a string contains only digits, i.e. is representing an int *)
let is_int str =
        let imax = (String.length str) in
        let rec aux i =
                if i >= imax then true
                else if ('0' <= str.[i]) && (str.[i] <= '9')
                        then aux (i+1)
                        else false
        in aux 0;;

(* checks if a string contains only digits (at least one) and exactly one dot, i.e. is representing a float *)
let is_float str =
        if str = "."
                then false
                else let splited_str = (String.split_on_char '.' str) in
                        match splited_str with
                        | a::b::[] -> (is_int a) && (is_int b)
                        | _ -> false;;

(* checks if prefix is a prefix of str (stolen in String lib Ocaml 4.13) *)
let starts_with prefix str =
        let len_str = String.length str
        and len_pref = String.length prefix in
        let rec aux i =
                if i = len_pref then true
                else if str.[i] <> prefix.[i] then false
                else aux (i + 1)
        in len_str >= len_pref && aux 0

(* -- String / Lexeme managment functions -- *)

(* converts a string into an Option of lexeme if it represents a lexeme, in None in the other case *)
let string_to_lexeme str =
        match str with
        | "(" -> Some L_bra
        | ")" -> Some R_bra
        | "int" -> Some Int_fun
        | "float" -> Some Float_fun
        | "+" -> Some Add_int
        | "-" -> Some Sub_int
        | "*" -> Some Mul_int
        | "/" -> Some Div_int
        | "%" -> Some Mod
        | "+." -> Some Add_float
        | "-." -> Some Sub_float
        | "*." -> Some Mul_float
        | "/." -> Some Div_float
        | "!" -> Some Fact
        | "^" -> Some Power
        | _ -> if is_int str
                then Some (Int(str))
                else if is_float str
                        then Some (Float(str))
                        else None;;

(* using a list of lexeme's string representation, check if a string is the prefix of some lexeme pattern *)
let is_a_lexeme_prefix str =
        if (is_float str) || (is_int str)
                then true
                else let lexeme_patterns = ["("; ")"; "int"; "float"; "+"; "-"; "*"; "+."; "-."; "*."; "/.";  "!"; "^"] in
                        let rec aux lst =
                                match lst with
                                | [] -> false
                                | t::q when (starts_with str t) -> true
                                | t::q -> aux q
                        in aux lexeme_patterns;;

(* ----- Main functions ----- *)

(* Lexical analyser : run the string and detect recognized lexemes to create the lexeme list *)
let lexical_analyser stringExp =
        let stringExpS = (skip stringExp [' ']) in (* the file string with no ' ' in it *)
        let imax = (String.length stringExpS) in
        let rec aux i buffer =
                let bufferLex = string_to_lexeme buffer in
                if i >= imax

                        (* final case *)
                        then if (Option.is_some bufferLex)
                                then [Option.get bufferLex]
                                else failwith ("'"^buffer^"' is not recognized")

                        (* general case -> buffer is added if it is a lexeme prefix but the next buffer is not *)
                        else let nextChar = (string_of_char stringExpS.[i]) in
                                let nextBuffer = (buffer ^ nextChar) in
                                if (is_a_lexeme_prefix nextBuffer)
                                        then aux (i+1) nextBuffer
                                        else if (Option.is_some bufferLex)
                                                then (Option.get bufferLex)::(aux (i+1) nextChar)
                                                else failwith ("'"^buffer^"' is not recognized")
        in if imax = 0
                then failwith "empty file"
                else aux 1 (string_of_char (stringExpS.[0]))

(* ----- Debug functions ----- *)

let string_of_lexeme lexeme =
        match lexeme with
        | L_bra -> "L_bra"
        | R_bra -> "R_bra"
        | Int_fun -> "Int_fun"
        | Float_fun -> "Float_fun"
        | Add_int -> "Add_int"
        | Sub_int -> "Sub_int"
        | Mul_int -> "Mul_int"
        | Div_int -> "Div_int"
        | Mod -> "Mod"
        | Add_float -> "Add_float"
        | Sub_float -> "Sub_float"
        | Mul_float -> "Mul_float"
        | Div_float -> "Div_float"
        | Fact -> "Fact"
        | Power -> "Power"
        | Int x -> "Int " ^ x
        | Float x -> "Float " ^ x

let print_list_lexeme lstlexeme =
        let rec aux lst =
                match lst with
                | [] -> "]\n"
                | t::q -> (string_of_lexeme t) ^ "; " ^ (aux q)
        in print_string ("[" ^ aux lstlexeme)


