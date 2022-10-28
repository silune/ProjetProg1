(* Main : generate de .s file using a .exp file *)

(* ----- Auxilary functions ----- *)

(* compile the string in the .exp file and write it in the .s file *)
let compile expString fileName =
        let lexemeList = Lexer.lexical_analyser expString in
        let typedTree = Parser.syntax_analyser lexemeList in
        let code = Compiler.assembly_of_tree typedTree in
        Compiler.write_code code fileName;;

(* read the file given in "input" and convert it into a string *)
let rec read_file input =
  try
    let l = input_line input in
    l ^ (read_file input)
  with | _ -> "";;

(* ----- Main functions ----- *)

let () =
        if Array.length Sys.argv <= 1 (* if no file is given in the ./aritha call *)
                then failwith "give some .exp file"
                else
                        let file = Sys.argv.(1) in
                        let fileName = (String.sub file 0 (String.index file '.')) in
                        if (fileName ^ ".exp") <> file (* if the file is not a .exp file *)
                                then failwith "give some .exp file"
                                else let fileInput = open_in file in compile (read_file fileInput) fileName;;
