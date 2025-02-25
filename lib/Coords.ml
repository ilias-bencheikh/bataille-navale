let char_to_int c =
  match c with
  | 'A' | 'a' -> 0
  | 'B' | 'b' -> 1
  | 'C' | 'c' -> 2
  | 'D' | 'd' -> 3
  | 'E' | 'e' -> 4
  | 'F' | 'f' -> 5
  | 'G' | 'g' -> 6
  | 'H' | 'h' -> 7
  | 'I' | 'i' -> 8
  | 'J' | 'j' -> 9
  | 'K' | 'k' -> 10
  | _ -> failwith "Invalid coordinates"

let rec is_empty list =
  match list with
  | ""::q -> is_empty q
  | [] -> true
  | _ -> false

let find_coords s =
  let list = String.split_on_char ' ' s in
  match list with
  | x::y::q -> 
    if not (is_empty q) then failwith "Invalid coordinates"
    else
    (try 
      let a = (char_to_int x.[0]) in
      let b = int_of_string y in
      (a, b)
    with Failure _ -> 
      try 
        let a = (int_of_string x) in
        let b = char_to_int y.[0] in
        (b, a)
      with Failure _ -> failwith "Invalid coordinates")
  | _ -> failwith "Invalid coordinates"