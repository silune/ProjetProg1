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

val lexical_analyser: string -> lexeme list
