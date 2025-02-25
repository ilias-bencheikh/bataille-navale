(* different bateau durant le jeux *) 
let navire_plateau_1 = ("PorteAvion",1,5)::("Croiseur",1,4)::("ContreTorpilleur",2,3)::("SousMarin",1,3)::("Torpilleur",1,2)::[]
let navire_plateau_2 = ("Torpilleur",1,2)::[]

(* affichage des regle du jeux depuis le menu*)
let afficher_regles () =
  print_endline "Règles de Bataille Navale :";
  print_endline "1. Chaque joueur place ses bateaux sur son plateau.";
  print_endline "2. Les joueurs tirent tour à tour en choisissant des coordonnées.";
  print_endline "3. Le but est de couler tous les bateaux de l'adversaire.";
  print_endline "4. Le premier joueur à couler tous les bateaux de l'autre gagne la partie.";
  print_endline "Appuyez sur entrée pour revenir au menu.";
  (try let x=read_line () in if x!="" then () with End_of_file ->  (print_endline "\027[31mVous avez quitté le jeu de façon non contrôlé\027[0m"); exit 0);