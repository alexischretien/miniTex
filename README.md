# miniTex

## Description

*miniTex* est un petit programme écrit en Haskell permettant de traiter des
fichiers textes contenant des commandes similaires à *LaTeX*. Trois types
de commandes sont traitées, permettant d'identifier des sections, des
figures et des tables. 

## Auteur

Alexis Chrétien

## Compilation et exécution

Via le compilateur *ghc* : 
```
ghc miniTex.hs
```
Exemple d'exécution du programme : 
```
./miniTex input.txt > output.txt
```
Ce qui produit le fichier *output.txt* contenant le texte formatté.

