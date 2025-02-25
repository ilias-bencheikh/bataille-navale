# Bataille Navale

Ce projet est une implémentation du jeu de la Bataille Navale en OCaml.

## Règle

La Bataille Navale est un jeu de stratégie où deux joueurs placent secrètement leurs navires sur une grille. Chaque joueur dispose d'une flotte composée de navires de différentes tailles. Les joueurs tirent à tour de rôle en annonçant des coordonnées pour tenter de toucher les navires adverses. Si un tir touche un navire, on annonce "touché", et lorsqu'un navire est entièrement touché sur toutes ses cases, il est "coulé". Le premier joueur qui parvient à couler tous les navires de son adversaire remporte la partie.
Ce jeu peut être également joué contre une IA 

## Prérequis

Les prérequis minimum pour lancer le jeu sont :

- OCaml(version 4.12 ou supérieure) installé sur votre machine.
- Dune installé pour gérer la compilation et l'exécution du projet.
- Opam pour gérer Ocaml et Dune

Cependant pour lancer les test vous aurez besoin de Qcheck installable avec cette commande :

```sh
opam install qcheck
```



## Installation

Clonez le dépôt :

```sh
git clone https://moule.informatique.univ-paris-diderot.fr/bencheik/Bataille_Navale.git
cd bataille_navale
```

## Utilisation

Pour lancer le jeu, exécutez la commande suivante :

- Pour les utilisateurs Unix:
```sh
chmod u+x ./run.sh 
./run.sh
```
- Pour les utilisateurs Windows (les meilleurs):
```sh
.\run.bat
```

## Contribuer

Les contributions sont les bienvenues ! Veuillez soumettre une pull request ou ouvrir une issue pour discuter des changements que vous souhaitez apporter.

## Auteur

- Ilias Bencheikh : @bencheik
- Rayan Belhassen : @belhasse

## Licence

Ce projet est sous licence @UnivParisCité.