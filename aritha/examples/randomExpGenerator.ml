let make_int_exp (res1Exp, res1Ml) (res2Exp, res2Ml) i =
        match i with
        | 0 -> ("int(" ^ res1Exp ^ ")", "(int_of_float (" ^ res1Ml ^ "))")
        | 1 -> ("(" ^ res1Exp ^ ")", "(" ^ res1Ml ^ ")")
        | 2 -> ("+(" ^ res1Exp ^ ")", "+(" ^ res1Ml ^ ")")
        | 3 -> ("-(" ^ res1Exp ^ ")", "-(" ^ res1Ml ^ ")")
        | 4 -> (res1Exp ^ "+" ^ res2Exp, res1Ml ^ " + " ^ res2Ml)
        | 5 -> (res1Exp ^ "*" ^ res2Exp, res1Ml ^ " * " ^ res2Ml)
        | 6 -> (res1Exp ^ "-" ^ res2Exp, res1Ml ^ " - " ^ res2Ml)
        | 7 -> (res1Exp ^ "/" ^ res2Exp, res1Ml ^ " / " ^ res2Ml)
        | 8 -> (res1Exp ^ "%" ^ res2Exp, res1Ml ^ " mod " ^ res2Ml)
        | _ -> failwith "impossible"

let make_float_exp (res1Exp, res1Ml) (res2Exp, res2Ml) i =
        match i with
        | 0 -> ("float(" ^ res1Exp ^ ")", "(float_of_int (" ^ res1Ml ^ "))")
        | 1 -> ("(" ^ res1Exp ^ ")", "(" ^ res1Ml ^ ")")
        | 2 -> ("+(" ^ res1Exp ^ ")", "+.(" ^ res1Ml ^ ")")
        | 3 -> ("-(" ^ res1Exp ^ ")", "-.(" ^ res1Ml ^ ")")
        | 4 -> ( res1Exp ^ "+." ^ res2Exp, res1Ml ^ " +. " ^ res2Ml)
        | 5 -> ( res1Exp ^ "*." ^ res2Exp, res1Ml ^ " *. " ^ res2Ml)
        | 6 -> ( res1Exp ^ "-." ^ res2Exp, res1Ml ^ " -. " ^ res2Ml)
        | 7 -> ( res1Exp ^ "/." ^ res2Exp, res1Ml ^ " /. " ^ res2Ml)
        | _ -> failwith "impossible"

let make_val maxVal resType =
        let i = Random.int 3 in
        match i with
        | 0 when resType = "int" -> string_of_int (Random.int maxVal)
        | 0 -> string_of_float (Random.float (float_of_int maxVal))
        | 1 when resType = "int" -> "+" ^ (string_of_int (Random.int maxVal))
        | 1 -> "+" ^ (string_of_float (Random.float (float_of_int maxVal)))
        | 2 when resType = "int" -> "-" ^ (string_of_int (Random.int maxVal))
        | 2 -> "-" ^ (string_of_float (Random.float (float_of_int maxVal)))
        | _ -> failwith "impossible"

let rec aux resType opCount maxVal =
        if opCount = 0
                then let res = (make_val maxVal resType) in (res, res)
                else if resType = "int"
                        then let i = Random.int 9 in
                                (match i with
                                | 0 -> let (resExp, resMl) = aux "float" (opCount - 1) maxVal in
                                        make_int_exp (resExp, resMl) ("", "") i
                                | k when k <= 3 -> let (resExp, resMl) = aux "int" (opCount - 1) maxVal in
                                                        make_int_exp (resExp, resMl) ("", "") i
                                | _ -> if opCount > 1 
                                                then let cut = Random.int (opCount) in
                                                        let (res1Exp, res1Ml) = aux "int" (opCount - 1 - cut) maxVal in
                                                        let (res2Exp, res2Ml) = aux "int" (cut) maxVal in
                                                        make_int_exp (res1Exp, res1Ml) (res2Exp, res2Ml) i
                                                else aux resType opCount maxVal)
                        else let i = Random.int 8 in
                                (match i with
                                | 0 -> let (resExp, resMl) = aux "int" (opCount - 1) maxVal in
                                        make_float_exp (resExp, resMl) ("", "") i
                                | k when k <= 3 -> let (resExp, resMl) = aux "float" (opCount - 1) maxVal in
                                                        make_float_exp (resExp, resMl) ("", "") i
                                | _ -> if opCount > 1
                                                then let cut = Random.int (opCount - 1) in
                                                        let (res1Exp, res1Ml) = aux "float" (opCount - 1 - cut) maxVal in
                                                        let (res2Exp, res2Ml) = aux "float" (cut) maxVal in
                                                        make_float_exp (res1Exp, res1Ml) (res2Exp, res2Ml) i
                                                else aux resType opCount maxVal)



let () =
        let fileName = if Array.length Sys.argv <= 1
                        then failwith "give a file name to write in"
                        else Sys.argv.(1) in
        let opCount = if Array.length Sys.argv <= 2
                        then failwith "give a number of operartion"
                        else int_of_string Sys.argv.(2) in
        let resType = if Array.length Sys.argv <= 3
                        then "int"
                        else Sys.argv.(3) in
        let maxVal = if Array.length Sys.argv <= 4
                        then 100
                        else int_of_string Sys.argv.(4) in
        let expFile = open_out (fileName ^ ".test.exp") and
                mlFile = open_out (fileName ^ ".answer.ml") in
        let (expCode, mlCode) = (Random.self_init (); aux resType opCount maxVal) in
        (Printf.fprintf expFile "%s\n" expCode;
        if resType = "int"
                then Printf.fprintf mlFile "%s\n" ("print_string ((string_of_int (" ^ mlCode ^ ")) ^ \"\n\")")
                else Printf.fprintf mlFile "%s\n" ("print_string ((string_of_float (" ^ mlCode ^ ")) ^ \"\n\")");
        close_out expFile;
        close_out mlFile)















