let array_to_list tab =
        let len = Array.length tab in
        let rec aux i =
                if i = len then []
                else tab.(i)::(aux (i+1))
        in aux 0
