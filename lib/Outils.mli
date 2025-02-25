val get : int -> 'a list -> 'a
val remove : int -> 'a list -> 'a list
val add : 'a -> 'a list -> 'a list
val length : 'a list -> int
val coordonnees_valides : int -> int -> int -> string -> int -> bool
val clearT : unit -> unit
val parse_coord : string -> int
val parse_coords : string list -> int * int
val iscoordonee : string list -> bool
