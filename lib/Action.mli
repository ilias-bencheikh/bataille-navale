type tir = Success of int * string | Error of string
val tirer : Plateaux.case array array -> int -> int -> tir
val demander_placement :
  string -> int -> Plateaux.case array array -> (int * int) list
val placer_tous_bateaux :
  Plateaux.case array array -> Bateaux.navire list ref -> unit
