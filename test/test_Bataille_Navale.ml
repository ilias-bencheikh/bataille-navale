open QCheck
open Outils
open Plateaux
open GameView
open Bateaux
open Action
open GameState

(* Générer des entiers entre 0 et 10 *)

(* Test pour la fonction get *)
let test_get =
  Test.make
    ~count:100
    ~name:"get i liste retourne le i-ème élément de la liste"
    (pair small_nat (list small_int))
    (fun (i, liste) -> 
      if i < List.length liste then 
        try Some (get i liste) = List.nth_opt liste i
        with _ -> false
      else true) (* Ignorer si l'index est hors limite *)

(* Test pour la fonction remove *)
let test_remove =
  Test.make
    ~count:100
    ~name:"remove i liste enlève le i-ème élément de la liste"
    (pair small_nat (list small_int))
    (fun (i, liste) ->
      if i < List.length liste then 
        let removed = remove i liste in
        List.length removed = List.length liste - 1 &&
        List.for_all (fun x -> List.mem x liste) removed
      else true) (* Ignorer si l'index est hors limite *)

(* Test pour la fonction add *)
let test_add =
  Test.make
    ~count:100
    ~name:"add x liste ajoute x au début de la liste"
    (pair small_int (list small_int))
    (fun (x, liste) -> 
      add x liste = x :: liste)

(* Test pour la fonction length *)
let test_length =
  Test.make
    ~count:100
    ~name:"length liste retourne la longueur de la liste"
    (list small_int)
    (fun liste -> 
      length liste = List.length liste)

(* Test pour la fonction coordonnees_valides *)
let test_coordonnees_valides =
  Test.make
    ~count:100
    ~name:"coordonnees_valides retourne true ou false correctement"
    (QCheck.tup5 small_nat small_nat small_nat (QCheck.oneofl ["h"; "v"]) small_nat)
    (fun (x, y, taille_bateau, orientation, plateau_taille) -> 
      coordonnees_valides x y taille_bateau orientation plateau_taille =
      (x >= 0 && y >= 0 && x < plateau_taille && y < plateau_taille &&
       ((orientation = "h" && y + taille_bateau <= plateau_taille) || 
        (orientation = "v" && x + taille_bateau <= plateau_taille)))) 



(* Test pour creer_plateau *)
let test_creer_plateau =
  Test.make
    ~count:100
    ~name:"creer_plateau crée un plateau de la taille spécifiée rempli de Vide"
    (small_nat)
    (fun taille ->
      let plateau = creer_plateau taille in
      Array.length plateau = taille &&
      Array.for_all (fun row -> Array.length row = taille && Array.for_all ((=) Vide) row) plateau)

(* Test pour verif_coord *)
let test_verif_coord =
  Test.make
    ~count:100
    ~name:"verif_coord vérifie que toutes les coordonnées sont Vide"
    (pair (list (pair small_nat small_nat)) (small_nat))
    (fun (coords, taille) ->
      let plateau = creer_plateau taille in
      (* Pour générer des coordonnées valides *)
      let coords_valides = List.filter (fun (x, y) -> x < taille && y < taille) coords in
      verif_coord coords_valides plateau)

(* Test pour placer_bateaux *)

let test_placer_bateaux =
  Test.make
    ~count:100
    ~name:"placer_bateaux place les navires aux coordonnées spécifiées"
    (pair (list (pair small_nat small_nat)) (list small_nat))
    (fun (coords, bateaux) ->
      let plateau = creer_plateau plateau_taille in
      let coords_valides = List.filter (fun (x, y) -> x < plateau_taille  && y < plateau_taille ) coords in
      placer_bateaux plateau coords_valides bateaux;
      List.for_all (fun (x, y) ->
        match plateau.(x).(y) with
        | Navire (_, Intact) -> true
        | _ -> false
      ) coords_valides)

(* Test pour coule *)
let test_coule =
  Test.make
    ~count:100
    ~name:"coule change les coordonnées spécifiées en Coule"
    (pair (list (pair small_nat small_nat)) (small_nat))
    (fun (coords, taille ) ->
      let plateau = creer_plateau taille in
      (* Placer des navires d'abord *)
      let coords_valides = List.filter (fun (x, y) -> x < taille  && y < taille ) coords in
      placer_bateaux plateau coords_valides [];
      coule coords_valides plateau;
      List.for_all (fun (x, y) ->
        match plateau.(x).(y) with
        | Coule -> true
        | _ -> false
      ) coords_valides)

(* Test pour update_etat *)
let test_update_etat =
  Test.make
    ~count:100
    ~name:"update_etat coule les navires si toutes les parties sont Touche"
    (pair (list (pair small_nat small_nat)) (small_nat))
    (fun (coords, taille) ->
      let plateau = creer_plateau taille in
      let coords_valides = List.filter (fun (x, y) -> x < taille && y < taille) coords in
      placer_bateaux plateau coords_valides [];
      (* Simuler que toutes les parties sont Touche *)
      List.iter (fun (x, y) -> plateau.(x).(y) <- Navire (1, Touche)) coords_valides;
      update_etat coords_valides plateau &&
      (List.for_all (fun (x, y) ->
        match plateau.(x).(y) with
        | Coule -> true
        | _ -> false
      ) coords_valides)
    )


(* Test pour afficher_plateau *)
let test_afficher_plateau =
  Test.make
    ~count:10
    ~name:"afficher_plateau s'exécute sans erreur"
    (small_nat)
    (fun taille ->
      let plateau = creer_plateau taille in
      try
        afficher_plateau plateau;
        true
      with _ -> false)

      (* Test pour afficher_plateau_gagner *)
let test_afficher_plateau_gagner =
  Test.make
    ~count:10
    ~name:"afficher_plateau_gagner s'exécute sans erreur"
    (small_nat)
    (fun taille ->
      let plateau = creer_plateau taille in
      try
        afficher_plateau_gagner plateau;
        true
      with _ -> false)

(* Test pour afficher_plateau_placement *)
let test_afficher_plateau_placement =
  Test.make
    ~count:10
    ~name:"afficher_plateau_placement s'exécute sans erreur"
    (small_nat)
    (fun taille ->
      let plateau = creer_plateau taille in
      try
        afficher_plateau_placement plateau;
        true
      with _ -> false)
      let test_tous_bateaux_coules =
        Test.make
          ~count:100
          ~name:"tous_bateaux_coules retourne true quand tous les bateaux sont coulés"
          (list (pair small_int small_nat))
          (fun coords ->
            let plateau = creer_plateau plateau_taille in
            (* Placer des bateaux et les marquer comme Coule *)
            let coords_valides = List.filter (fun (x, y) -> x < plateau_taille && y < plateau_taille) coords in
            placer_bateaux plateau coords_valides [];
            coule coords_valides plateau;
            tous_bateaux_coules plateau= true)
      
      let test_tous_bateaux_non_coules =
        Test.make
          ~count:100
          ~name:"tous_bateaux_coules retourne false quand au moins un bateau est non coulé"
          (pair small_nat (list (pair small_int small_nat)))
          (fun (taille, coords) ->
            let plateau = creer_plateau taille in
            let coords_valides = List.filter (fun (x, y) -> x < taille && y < taille) coords in
            placer_bateaux plateau coords_valides [];
            if coords_valides = [] then
             true else 
            tous_bateaux_coules plateau == false)

  (* Test pour la fonction tirer *)

let test_tirer_manque =
  Test.make
    ~count:100
    ~name:"tirer retourne Success(-1,\"Manqué!\") et marque Rate quand il manque"
    (pair (int_bound 10) (int_bound 10))  (* Génère des coordonnées x, y entre 0 et 10 *)
    (fun (x, y) ->
      let plateau = creer_plateau plateau_taille in
      (* Assure-toi que la case est Vide *)
      plateau.(x).(y) = Vide &&
      (* Applique la fonction tirer *)
      let result = tirer plateau x y in
      (* Vérifie le résultat et l'état de la case *)
      result = Success(-1,"Manqué!") && plateau.(x).(y) = Rate)

let test_tirer_touche =
  Test.make
    ~count:100
    ~name:"tirer retourne Success(1,\"\027[33mTouché!\027[0m\") quand un bateau est touché"
    (pair (int_bound 10) (int_bound 10))
    (fun (x, y) ->
      let plateau = creer_plateau plateau_taille in
      (* Place un bateau à (x, y) avec id=1 et état Intact *)
      plateau.(x).(y) <- Navire (1, Intact);
      (* Applique la fonction tirer *)
      let result = tirer plateau x y in
      (* Vérifie le résultat et l'état de la case *)
      result = Success(1,"\027[33mTouché!\027[0m") && plateau.(x).(y) = Navire (1, Touche))

let test_tirer_deja_touche =
  Test.make
    ~count:100
    ~name:"tirer retourne Error (\"Déjà touché.\") quand la case a déjà été touchée"
    (pair (int_bound 10) (int_bound 10))
    (fun (x, y) ->
      let plateau = creer_plateau plateau_taille in
      (* Place un bateau à (x, y) avec état Touche *)
      plateau.(x).(y) <- Navire (1, Touche);
      (* Applique la fonction tirer *)
      let result = tirer plateau x y in
      (* Vérifie le résultat *)
      result = Error ("Déjà touché."))

let test_tirer_deja_coule =
  Test.make
    ~count:100
    ~name:"tirer retourne Error(\"Déjà coulé.\") quand la case est déjà coulée"
    (pair (int_bound 10) (int_bound 10))
    (fun (x, y) ->
      let plateau = creer_plateau plateau_taille in
      (* Place un bateau à (x, y) puis le couler *)
      plateau.(x).(y) <- Coule;
      (* Applique la fonction tirer *)
      let result = tirer plateau x y in
      (* Vérifie le résultat *)
      result = Error("Déjà coulé."))


let () =
  QCheck_runner.run_tests_main [
    test_get;
    test_remove;
    test_add;
    test_length;
    test_coordonnees_valides;
    test_creer_plateau;
    test_verif_coord;
    test_placer_bateaux;
    test_coule;
    test_update_etat;
    test_afficher_plateau;
    test_afficher_plateau_gagner;
    test_afficher_plateau_placement;
    test_tous_bateaux_coules;
    test_tous_bateaux_non_coules;
    test_tirer_manque;
    test_tirer_touche;
    test_tirer_deja_touche;
    test_tirer_deja_coule;

  ]
