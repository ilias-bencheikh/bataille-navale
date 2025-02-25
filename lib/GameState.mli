val tous_bateaux_coules : Plateaux.case array array -> bool
val tours_joueur :
  int ->
  Plateaux.case array array -> Bateaux.navire list ref -> string ref -> bool
val tours_ia :
  Plateaux.case array array -> Bateaux.navire list ref -> string ref -> bool
val jeu : unit -> unit
