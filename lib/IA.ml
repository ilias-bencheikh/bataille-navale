open Regle
open Bateaux
open Outils
open Plateaux
open Action

(* liste des tirs effectués *)
let tirs_effectues = ref []

(* première position touchée *)
let premier_touche = ref None

(* dernière position touchée *)
let dernier_touche = ref None

(* réinitialise les tirs en cours *)
let reset_tirs () =
  dernier_touche := None

(* tire de manière aléatoire une position qui n'a pas encore été tirée *)
let rec tir_aleatoire () =
  let x = Random.int plateau_taille in
  let y = Random.int plateau_taille in
  if List.mem (x, y) !tirs_effectues then
    tir_aleatoire () (* réessaie si la position a déjà été tirée *)
  else
    (x, y)

(* fonction pour tirer sur le plateau avec l'IA *)
let ia_tirer plateau =
  let x, y =
    let rec tire_deja_touche () =
      match !dernier_touche with
      | Some (dx, dy) ->
          (* calcule les positions adjacentes *)
          let adjacents = [(dx + 1, dy); (dx - 1, dy); (dx, dy + 1); (dx, dy - 1)] in
          (* filtre les positions adjacentes valides et non encore tirées *)
          let valid_adj = List.filter (fun (x, y) ->
            x >= 0 && x < plateau_taille &&
            y >= 0 && y < plateau_taille &&
            not (List.mem (x, y) !tirs_effectues)
          ) adjacents in
          if valid_adj = [] then (
            (* si aucune position adjacente n'est valide, revenir à la première touche *)
            dernier_touche := !premier_touche;
            tire_deja_touche ()
          ) else
            (* choisit une position aléatoire parmi les adjacentes valides *)
            (get (Random.int (List.length valid_adj)) valid_adj )
      | None ->
          tir_aleatoire () (* tire aléatoirement si aucune touche précédente *)
    in
    tire_deja_touche ()
  in
  tirs_effectues := (x, y) :: !tirs_effectues;
  match plateau.(x).(y) with
  | Vide ->
      plateau.(x).(y) <- Rate;
      Success (-1, "L'IA a raté!")
  | Coule ->
      Error "Déjà coulé."
  | Rate ->
      Error "Déjà tiré ici."
  | Navire (id, etat) ->
      (match etat with
       | Intact ->
           plateau.(x).(y) <- Navire (id, Touche);
           dernier_touche := Some (x, y);
           Success (id, "L'IA a touché un bateau!")
       | Touche ->
           Error "Déjà touché.")

(* placement de bateau aléatoirement *)
let ajoute_bateau_alea nom taille plateau liste_navire =
  let rec placer () = 
    (* génère des coordonnées aléatoires et une orientation *)
    let x = Random.int (plateau_taille - 1) in
    let y = Random.int (plateau_taille - 1) in
    let orientation = if Random.int 2 = 0 then "h" else "v" in
    if coordonnees_valides x y taille orientation plateau_taille then
      let coords = (
        print_int x;
        print_string ", ";
        print_int y;
        print_string ", ";
        print_int taille;
        print_string ", ";
        print_string orientation;
        print_newline();
        make_pos_list x y taille orientation
      ) in
      if verif_coord coords plateau then (
        (* place le bateau sur le plateau *)
        placer_bateaux plateau coords !liste_navire;
        (* ajoute le navire à la liste des navires *)
        liste_navire := (make_navire nom ((List.length !liste_navire) + 1) coords) :: !liste_navire;
      ) else
        placer () (* réessaie si les coordonnées ne sont pas valides *)
    else
      placer () (* réessaie si les coordonnées ne sont pas valides *)
  in
  placer ()

(* placement de tous les bateaux de l'IA *) 
let placer_tous_bateaux_ia plateau liste_navire = 
  let navires = navire_plateau_1 in
  List.iter (fun (nom, nb, taille) -> 
    let rec place n = 
      match n with 
      | 0 -> ()
      | _ ->
          ajoute_bateau_alea nom taille plateau liste_navire;
          place (n - 1)
    in
    place nb
  ) navires;
  ()
