(* fonction pour obtenir l'élément à l'index i dans une liste *)
let rec get i liste = 
  match liste with
  | [] -> failwith "Sortie de liste"  (* erreur si l'index dépasse la taille de la liste *)
  | h::q -> if i = 0 then h else get (i-1) q

(* fonction pour supprimer l'élément à l'index i dans une liste *) 
let rec remove i liste =
  match liste with
  | [] -> []  (* retourne une liste vide si l'index dépasse la taille *)
  | h::q -> if i = 0 then q else h::(remove (i-1) q)

(* fonction pour ajouter un élément en tête de liste *)
let add x liste = x::liste

(* fonction récursive pour calculer la longueur d'une liste *)
let rec length liste =
  match liste with
  | [] -> 0
  | _::q -> 1 + (length q)

(* fonction pour vérifier si les coordonnées sont valides *)
let coordonnees_valides x y taille_bateau orientation plateau_taille =
  if x < 0 || y < 0 || x >= plateau_taille || y >= plateau_taille then false
  else if orientation = "h" || orientation = "H" then
    (y + taille_bateau <= plateau_taille)  (* vérifie si le bateau ne dépasse pas horizontalement *)
  else if orientation = "v" || orientation = "V" then
    (x + taille_bateau <= plateau_taille)  (* vérifie si le bateau ne dépasse pas verticalement *)
  else
    false  (* orientation invalide *)

(* fonction pour nettoyer le terminal *)
let clearT () =
  let command =
    if Sys.os_type = "Win32" then "cls"  (* commande pour Windows *)
    else "clear"  (* commande pour Unix/Linux/Mac *)
  in
  ignore (Sys.command command)

(* fonction pour analyser une coordonnée individuelle *)
let parse_coord coord_str =
  print_endline "Parsed a coord";
  try
    int_of_string coord_str  (* tente de convertir en entier *)
  with Failure _ ->
    if String.length coord_str = 1 then
      let c = coord_str.[0] in
      if c >= 'a' && c <= 'z' then
        (Char.code c) - (Char.code 'a')  (* convertit une lettre minuscule en index *)
      else if c >= 'A' && c <= 'Z' then
        (Char.code c) - (Char.code 'A')  (* convertit une lettre majuscule en index *)
      else
        failwith "Coordonnée invalide"  (* caractère non valide *)
    else
      failwith "Coordonnée invalide"  (* longueur incorrecte *)

(* fonction pour analyser une paire de coordonnées *)
let parse_coords coords_split =
  (* vérifie si un caractère est une lettre *)
  let est_lettre c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  in
  (* vérifie si une chaîne représente un chiffre valide *)
  let est_chiffre s =
    try
      ignore (int_of_string s); true
    with Failure _ -> false
  in
  (* vérifie et ajuste l'ordre des coordonnées *)
  let verifier_et_inverser coords_split =
    print_endline "Vérification";
    match coords_split with
    | [a; b] when est_lettre a.[0] && est_chiffre b -> 
        print_endline "Lettre - Chiffre 1"; coords_split
    | [b; a] when est_lettre a.[0] && est_chiffre b -> 
        print_endline "Lettre - Chiffre 2"; [a; b]
    | [a; b] when est_chiffre a && est_chiffre b -> 
        print_endline "Chiffre - Chiffre "; coords_split
    | [a; b] when est_lettre a.[0] && est_lettre b.[0] -> 
        print_endline "Lettre - Lettre "; coords_split
    | _ -> failwith "Format de coordonnées incorrect"
  in
  let coords_split = verifier_et_inverser coords_split in
  match List.map parse_coord coords_split with
  | [a; b] when a >= 0 && a < 26 && b >= 0 -> (a, b)  (* retourne les coordonnées si valides *)
  | _ -> failwith "Format de coordonnées incorrect"

(* fonction pour vérifier si une liste de chaînes représente des coordonnées valides *)
let iscoordonee coords = 
  match coords with
  | [x_str; y_str] ->
      String.length x_str == 1 &&
      (String.length y_str == 1 || String.length y_str == 2) &&
      x_str.[0] >= 'A' && x_str.[0] <= 'Z' &&
      ((y_str.[0] >= '0' && y_str.[0] <= '9') || y_str = "10")
  | _ -> false
