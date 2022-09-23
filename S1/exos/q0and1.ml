let list_to_array l =
        if l = [] then [||]
        else let len = List.length l and elem = List.hd l in
                let res = Array.make len elem in
                let rec aux i ll = match ll with
                        |[] -> res
                        |t::q -> res.(i) <- t; aux (i+1) q
        in aux 0 l
                
