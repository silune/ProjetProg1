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

val lexical_analyser: string -> lexeme list
val print_list_lexeme: lexeme list -> unit
