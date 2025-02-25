val ajoute_bateau_alea :
  string ->
  int -> Plateaux.case array array -> Bateaux.navire list ref -> unit
val placer_tous_bateaux_ia :
  Plateaux.case array array -> Bateaux.navire list ref -> unit
val tirs_effectues : (int * int) list ref
val premier_touche : (int * int) option ref
val dernier_touche : (int * int) option ref
val reset_tirs : unit -> unit
val tir_aleatoire : unit -> int * int
val ia_tirer : Plateaux.case array array -> Action.tir
