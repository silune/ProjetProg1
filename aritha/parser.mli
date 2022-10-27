open Lexer

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
        | FACT of tast
        | POWERI of tast * tast
        | INT of string
        | FLOAT of string

val syntax_analyser: lexeme list -> tast
val type_of_tast: tast -> string
