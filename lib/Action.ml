(* Action.ml *)
open Plateaux
open Bateaux
open Outils
open GameView
open Regle
open Coords

type tir = Success of int * string | Error of string   
(* Success de l'id du bateau touché et message de succès, Error de message d'erreur *)

(* Fonction pour tirer sur une case *)
let tirer plateau x y =
  let taille = Array.length plateau in
  if x < 0 || x >= taille || y < 0 || y >= taille then
    (Error("\027[31mCoordonnées invalides, réessayez.\027[0m")) (*Cas ou le joueur doit recommencer*)
  else
    match plateau.(x).(y) with
    | Vide ->
        plateau.(x).(y) <- Rate;
        Success(-1,"Manqué!")
    | Coule ->
        Error ("Déjà coulé.")
    | Rate ->
        Error ("Déjà tiré ici.")
    | Navire (id, etat) ->
        match etat with
        | Intact ->
            plateau.(x).(y) <- Navire (id, Touche);
            Success(id, "\027[33mTouché!\027[0m")
        | Touche ->
            Error ("Déjà touché.")

  (* Fonction pour demander au joueur de placer le bateau *)
  let demander_placement nom taille plateau =
    afficher_plateau_placement plateau; (* Afficher le plateau *)
    let rec demander_valides () =
      print_endline (Printf.sprintf "Placer le %s (taille: %d)" nom taille);
      print_endline "Entrez les coordonnées de départ:";
      (try let coords = try read_line () with End_of_file -> raise (Failure "End_of_file") in (* Trouver les coordonnées *)
      try let (x,y) = find_coords coords in
      print_endline "Entrez l'orientation (h pour horizontal, v pour vertical) :";
      let orientation = try read_line () with End_of_file -> raise (Failure "End_of_file") in
      if coordonnees_valides x y taille orientation plateau_taille then (* Vérifier si les coordonnées sont valides *)
        let positions = make_pos_list x y taille orientation in (* Créer la liste des positions *)
        let coords = List.map (fun (x, y) -> (x, y)) positions in (* Convertir les positions en coordonnées *)
        if verif_coord coords plateau then 
          (clearT (); positions (* Coordonnées valides et bateau peut être placé *))
        else begin
          print_endline "\027[31mLes coordonnées sont valides mais il y a déjà un bateau à cet endroit.\027[0m";
          demander_valides () (* Redemander placement *)
        end
      else begin
        print_endline "\027[31mLes coordonnées ou l'orientation sont invalides, veuillez réessayer.\027[0m";
        demander_valides () (* Redemander placement *)
      end
    with Failure (_) -> 
    (print_endline "\027[31mEntrée invalide, réessayez.\027[0m";
    demander_valides ()) (* Redemander placement *)
    with Failure (_) ->
    (print_endline "\027[31mVous avez quitté le jeu de façon non contrôlé\027[0m"); exit 0);
    in
    demander_valides ()


(* Placer tous les bateaux *)
let rec placer_tous_bateaux plateau list_bateaux=
    let navires = navire_plateau_1 in
    List.iter (fun (nom, nb ,taille)-> 
      let rec place n = 
        match n with
        | 0 -> ()
        | _ -> let coords = demander_placement nom taille plateau in 
                let coords = List.map (fun (x, y) -> (x, y)) coords in
                ((placer_bateaux plateau coords !list_bateaux);list_bateaux:=((make_navire nom ((length !list_bateaux)+1) coords))::!list_bateaux); place (n-1)
      in place nb ;
    ) navires ; 
    afficher_plateau_placement plateau;
    print_endline "(R pour replacer, sinon appuyer sur entrée) :";
    let  orientation = read_line () in
    if orientation =  "r" || orientation = "R" then (reset_plateaux plateau list_bateaux ; placer_tous_bateaux plateau list_bateaux)
    else ()
    

  
  