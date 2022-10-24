open Parser
open X86_64

val assembly_of_tree: tast -> program
val write_code: program -> string -> unit
