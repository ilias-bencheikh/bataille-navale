type nom_navire =
    Cuirasse
  | Croiseur
  | Torpilleur
  | SousMarin
  | PorteAvion
  | ContreTorpilleur
type etat_navire = Intact | Touche
type navire = { nom : nom_navire; id : int; coord : (int * int) list; }
type list_navire = navire list
val make_navire : string -> int -> (int * int) list -> navire
val make_pos_list : int -> int -> int -> string -> (int * int) list
val get_coord : int -> navire list -> (int * int) list
