open Bateaux
open Plateaux
open Action
open GameView
open Outils
open IA
open Regle
open Coords

(* Fonction pour vérifier si tous les bateaux sont coulés *)
let tous_bateaux_coules plateau =
  let coules = ref true in
  Array.iter (fun ligne ->
      Array.iter (fun case ->
          match case with
          | Navire (_, Intact) -> coules := false
          | Navire (_,Touche) -> coules := false
          | _ -> ()
        ) ligne
    ) plateau;
  !coules


(* Fonction pour placer tous les bateaux *)  
let tours_joueur j plateau_joueur liste_bateaux affichage = 
    if (!affichage!="") then print_endline !affichage else ();
    print_endline ("Au tour du joueur "^ string_of_int j ^" de jouer");
    print_endline "Entrez les coordonnées de tir:";
    let coords = read_line () in
    try let (x,y) = find_coords coords in
          (match tirer plateau_joueur x y  with
            | Success(i,s) ->
              begin
                affichage := s;
                if i >= 0 then
                  (if (update_etat (get_coord i !liste_bateaux) plateau_joueur) then (affichage := "\027[32mCoulé !\027[0m"; true)
                  else true)
                else false 
              end
            | Error(s) -> affichage := s; false;);
      with Invalid_argument(_) -> (affichage:= "\027[31mEntrée invalide, réessayez.\027[0m"; true;)

      
let tours_ia plateau_joueur liste_bateaux affichage =
  print_endline "Tour de l'IA";
  match ia_tirer plateau_joueur with
  | Success(i,s) ->
    begin
      affichage := !affichage ^ "\n" ^s;
      if i >= 0 then
        (if (update_etat (get_coord i !liste_bateaux) plateau_joueur) then (affichage := "\027[32mCoulé !\027[0m"; true)
        else true)
      else false 
    end
  | _ -> true

(* Fonction principale du jeu *)
let rec jeu () =
  (* Initialisation de la random seed *)
  Random.self_init ();
  (* Affichage du menu *)
  let x = afficher_Menu () in
  (* Initialisation du jeu *)
  let () = print_endline "Bienvenue dans le jeu de bataille navale!" in
  let menu () =
      clearT ();
      if x = "3" then 
       (afficher_regles ();jeu ())
      else ();
      if not (x = "0") && not (x = "1") && not (x = "3")  then jeu() else ();
  in menu ();
  (* Initialisation du jeu *)
      let joueur1 = "1" in
      let joueur2 = if(int_of_string x == 1)then "IA" else "2" in

      let liste_bateaux_joueur_1 = ref [] in
      let liste_bateaux_joueur_2 = ref [] in
      let plateau_joueur_1 = creer_plateau plateau_taille in
      let plateau_joueur_2 = creer_plateau plateau_taille in
      (* Placement des bateaux *)
      print_endline ("Au tour du joueur " ^ joueur1 ^" de placer ces bateaux");
      placer_tous_bateaux plateau_joueur_1 liste_bateaux_joueur_1;
      clearT ();
      print_endline ("Au tour du joueur "^joueur2^" de placer ces bateaux");
      if joueur2 = "IA" then placer_tous_bateaux_ia plateau_joueur_2 liste_bateaux_joueur_2
      else placer_tous_bateaux plateau_joueur_2 liste_bateaux_joueur_2;
      clearT ();
  let rec return_menu () =
    print_endline "Voulez-vous retourner au menu? (o/n)";
    let choix = read_line () in
    if choix = "o" || choix="O" then jeu () else if choix= "n" || choix=="N" then clearT () else (print_endline "Entrée invalide"; return_menu ());
  in
  let affichage = ref "" in
  (* Boucle principale du jeu *)
  let rec boucle j =
    if tous_bateaux_coules plateau_joueur_1 then
      (afficher_plateau_gagner plateau_joueur_1;
      print_endline ("\027[32mTous les bateaux du joueur "^joueur1^" ont été coulés! Le joueur "^joueur2^" a gagne!\027[0m"); return_menu ())
    else if tous_bateaux_coules plateau_joueur_2 then
      (afficher_plateau_gagner plateau_joueur_2;
        print_endline ("\027[32mTous les bateaux du joueur "^joueur2^" ont été coulés! Le joueur "^joueur1^" a gagne!\027[0m");return_menu ())
    else 
      if j==1 then 
        if (if joueur2=="IA" then (afficher_plateaux_cotes_a_cotes plateau_joueur_2 plateau_joueur_1; tours_joueur j plateau_joueur_2 liste_bateaux_joueur_2 affichage) else (afficher_plateau plateau_joueur_2;tours_joueur j plateau_joueur_2 liste_bateaux_joueur_2 affichage)) then boucle 1 else boucle 2
        else if j==2 then if joueur2=="IA" then 
          if (tours_ia plateau_joueur_1 liste_bateaux_joueur_1 affichage) then boucle 2 else boucle 1
          else
          if (afficher_plateau plateau_joueur_1 ;(tours_joueur j plateau_joueur_1 liste_bateaux_joueur_1 affichage))then boucle 2 else boucle 1
  in
  boucle 1; 

