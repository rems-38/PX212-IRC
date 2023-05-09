# Séance 1 - 20/03/2023

## Contenu de la séance
- Prise de connaissance du sujet
- Découverte de l'AES
## Pour la prochaine séance
### Pour Rémi
- Implémentation des opérations de base (partie 4)
- Etude approfondie de la specification
### Pour Guillemot
- Mise de place de tests unitaires
- Etude approfondie de la specification

# Travail entre les séances 1 et 2
- Etude de la partie 4 de la spécification
- Implémentation des fonctions :
    - xor : addition modulo 2
    - oppose2 : opposé modulo 2
    - toZ2Z : utile pour les notations
    - toPoly_Z2Z : passage d'un tableau d'entiers à un polynôme
    - addition_poly : addition de polynômes
    - multiplication_poly : multiplication de polynômes
    - (+ toutes les fonctions auxiliaires nécessaires/sous-jacentes)
- Début d'implémentation de la fonction d'inverse

# Séance 2 - 31/03/2023
## Contenu de la séance
- Mise en place du GitLab
- Ajout des structures algébriques
- Début de mise en place des fichiers ghci pour les tests
- Remise en forme de la structure des fichiers du projet

## Pour la prochaine séance
### Pour Rémi
- Fin d'implémentation des structures algébriques
- Instanciation des structures algébriques (pour les polynomes à coefficients dans Z2Z)
- Adaptation des fonctions réalisées précédemment pour coller avec les nouvelles structures implémentées
- Etude de la fin du document FIPS
### Pour Guillemot
- Fin de mise en place des fichiers de test ghci pour les fonctions mathématiques, ZsurNZ et les structures algébriques
- Etude de la fin du document FIPS
- Rédaction d'un document résumant la spécification

# Travail entre les séances 2 et 3
- Fin d'implémentation de toutes les structures algébriques (groupe, anneau, corps)
- Rajout d'un classe de polynômes irréductibles pour la multiplication de polynômes
- Instanciation des structures algébriques : Z_sur_2Z (en tant que Corps) et polynomes à coefs. dans Z_sur_2Z (en tant qu'anneau [pour l'instant l'inverse n'a pas été défini])
- Petites corrections dans les fichiers de tests (car changements de nom ou suprresions de fonction)

## Pour la prochaine séance
### Pour Rémi
- Documentation du code
- Réalisation de tests unitaires
- Commencement des fonctions de chiffrement