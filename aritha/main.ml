let compile expString fileName =
        let lexemeList = Lexer.lexical_analyser expString in
        let typedTree = Parser.syntax_analyser lexemeList in
        let code = Compiler.assembly_of_tree typedTree in
        Compiler.write_code code fileName;;

let rec read_file input =
  try
    let l = input_line input in
    l ^ (read_file input)
  with | _ -> "";;

let () =
        if Array.length Sys.argv <= 1
                then failwith "give some .exp file"
                else
                        let file = Sys.argv.(1) in
                        let fileName = (String.sub file 0 (String.index file '.')) in
                        if (fileName ^ ".exp") <> file
                                then failwith "give some .exp file"
                                else let fileInput = open_in file in compile (read_file fileInput) fileName;;
