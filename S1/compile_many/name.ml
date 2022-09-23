type name = string * string

let first_name n = fst n
let last_name n = snd n

let check_name n = true
 
let string_of_name n =
  if check_name n then (first_name n) ^ " " ^ (last_name n) else "Bad name"

