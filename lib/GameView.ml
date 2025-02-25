open Bateaux
open Plateaux
open Outils


let affiche_indice taille =
  let rec aff taille i =
    if i<taille then (Printf.printf "%d " i;aff taille (i+1)) else ()
  in aff taille 0

(* Affichage du plateau *)
let afficher_plateau plateau =
  clearT ();
  let taille = Array.length plateau in
  (* Afficher les indices de colonnes *)
  print_string "  ";
  affiche_indice taille;
  print_newline ();

  (* Afficher les lignes du plateau *)
  Array.iteri (fun i ligne ->
      Printf.printf "%c " (char_of_int (i + int_of_char 'A'));
      Array.iter (fun case ->
          match case with
          | Vide -> print_string "🟦"
          | Rate -> print_string "⬜"
          | Coule -> print_string "🟩"
          | Navire (_,e)-> match e with
                    | Touche -> print_string "🟨"
                    | _ -> print_string "🟦"
        ) ligne;
      print_newline () 
    ) plateau
    
(* Affichage du plateau de l'IA et du joueur côte à côte *)
  let affiche_ligne plateau_ia plateau_joueur taille =
    let rec aff i =
      if i<taille-1 then 
        begin
        Printf.printf "%c " (char_of_int (i + int_of_char 'A'));
        Array.iter (fun case ->
            match case with
            | Vide -> print_string "🟦"
            | Rate -> print_string "⬜"
            | Coule -> print_string "🟩"
            | Navire (_, e) -> match e with
              | Touche -> print_string "🟨"
              | _ -> print_string "🟦"
          ) plateau_ia.(i) ;
        print_string "   |   " ;
        Printf.printf "%c " (char_of_int (i + int_of_char 'A'));
        Array.iter (fun case ->
            match case with
            | Vide -> print_string "🟦"
            | Rate -> print_string "⬜"
            | Coule -> print_string "🟥"
            | Navire (_, e) -> match e with
              | Touche -> print_string "🟨"
              | _ -> print_string "🟩"
          ) plateau_joueur.(i);
        print_newline ();
        aff (i+1);
        end
      else print_newline ()
    in aff 0
  
  let afficher_plateaux_cotes_a_cotes plateau_ia plateau_joueur =
      clearT ();
      let taille = Array.length plateau_ia in
      (* Afficher les indices de colonnes pour les deux plateaux *)
      print_string "  ";
      affiche_indice taille;
      print_string "  |     ";
      affiche_indice taille;
      print_newline ();
      (* Afficher les lignes des deux plateaux *)
      affiche_ligne plateau_ia plateau_joueur taille

let afficher_espace () =
  print_endline "-----------------------------------"
(* Affichage du plateau quand tu a gagner  *)
let afficher_plateau_gagner plateau =
  clearT ();
  let taille = Array.length plateau in
  (* Afficher les indices de colonnes *)
  print_string "  ";
  for i = 0 to taille - 1 do
    Printf.printf "%d " i
  done;
  print_newline ();

  (* Afficher les lignes du plateau *)
  Array.iteri (fun i ligne ->
      Printf.printf "%c " (char_of_int (i + int_of_char 'A'));
      Array.iter (fun case ->
          match case with
          | Coule-> print_string "🟩"
          | _ -> print_string "⬜"
        ) ligne;
      print_newline ()
    ) plateau;
  print_endline ""

let afficher_plateau_placement plateau =
  let taille = Array.length plateau in
  print_string "  ";
  for i = 0 to taille - 1 do
    Printf.printf "%d " i
  done;
  print_newline ();

  (* Afficher les lignes du plateau *)
  Array.iteri (fun i ligne ->
      Printf.printf "%c " (char_of_int (i + int_of_char 'A'));
      Array.iter (fun case ->
          match case with
          | Navire (_,_)-> print_string "🟪"
          | _ -> print_string "🟦"
        ) ligne;
      print_newline ()
    ) plateau

let afficher_Menu () =
  clearT ();
  print_endline"";
  print_endline"                           ⬛​⬜";
  print_endline"                ⬛​         ⬛​⬜⬜";
  print_endline"                ⬛​⬜       ⬛​⬜⬜⬜⬜";
  print_endline"                ⬛​⬜⬜     ⬛​";
  print_endline"        ⬛​      ⬛​         ⬛​⬜⬜";
  print_endline"        ⬛​⬜​    ⬛​⬜​       ⬛​⬜⬜⬜";
  print_endline"        ⬛​⬜​    ⬛​⬜​⬜     ⬛​⬜⬜⬜⬜";
  print_endline"        ⬛​⬜​⬜​  ⬛​⬜​⬜​⬜​   ​⬛​⬜⬜⬜⬜⬜";
  print_endline"        ⬛​      ⬛​         ⬛​";
  print_endline"    ⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​​";
  print_endline"        ⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​​";
  print_endline"          ⬛​⬛​🟨⬛​🟨⬛​⬛​🟨⬛​🟨⬛​⬛​🟨⬛​⬛​​";
  print_endline"            ⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​​​";
  print_endline"            ⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​​";
  print_endline"🟦🟦🔵​🟦🟦🔵​🟦🔵​🟦🟦🟦🟦🟦🟦🟦🟦🔵​🟦🔵​🟦​🟦​🟦";
  print_endline"🟦🟦🔵​🟦🟦🔵​🟦🔵​🟦🟦🟦🟦🟦🟦🟦🟦🔵​🟦🔵​🟦​🟦​🟦";
  print_endline"🔲​⬛​⬛​​⬛​​​⬛​⬛​⬛​​​⬛​⬛​​⬛​​​⬛​⬛​​⬛​⬛​⬛​​⬛​​​⬛​⬛​⬛​​​⬛​⬛​🔲​";
  print_endline"⬛                                        ​⬛";
  print_endline"⬛ 0️⃣  :1 vs 1​​                             ⬛";
  print_endline"⬛                                        ​⬛";
  print_endline"⬛ 1️⃣​​  :1 vs IA​                            ⬛";
  print_endline"⬛                                        ​⬛";
  print_endline"⬛ ​2️⃣​  :1 vs 1 rapide​ 🔒                   ⬛";
  print_endline"⬛                                        ​⬛";
  print_endline"⬛ 3️⃣​  :regle​                              ⬛"; 
  print_endline"⬛                                        ​⬛";
  print_endline"🔲​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​⬛​🔲";
  
  (*
  print_endline "🚢​ 🛥️ ​⛴️ ​🛳️ ​🚤 ​⛵ ​🛶​";
  print_endline "Bienvenue dans la bataille navale!";
  print_endline "0️⃣​ Jouer";
  print_endline "1️⃣ Règles";
  print_endline "2️⃣​ Quitter";
  print_string "Choix: ";
  *)
  (try let x = read_line () in x with End_of_file ->  (print_endline "\027[31mVous avez quitté le jeu de façon non contrôlé\027[0m"); exit 0);
  

