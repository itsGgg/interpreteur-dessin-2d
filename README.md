# Interpréteur d'un langage de dessin 2D

## Compilation et exécution

```
dune clean               # Nettoie les fichiers de compilation
dune build                # Compile le projet
dune exec -- main.exe < fichier   # Lance l'interpréteur
```

(ou directement : `_build/default/main.exe < fichier`)

## Parties implémentées (Étapes 1 à 4)

- Étape 1 : Lexer, parser, interpréteur et fenêtre Graphics
- Étape 2 : Couleurs et largeur pinceau
- Étape 3 : Variables (déclaration, initialisation, vérification de portée et d'usage)
- Étape 4 : Si/Alors, Si/Alors/Sinon, Tant que, Repeter, blocs Debut/Fin et conditions booléennes

## Tests et Exemples

- `/exemples` : Exemples fournis dans le sujet et ajouts personnels illustrant les fonctionnalités
- `/tests` : Autres exemples/fichiers de test
