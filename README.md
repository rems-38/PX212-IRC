# Chiffrement AES

Ce projet consiste à réaliser une implémentation du chiffrement AES. L'implémentation se déroule en 2 temps : une première version en Haskell, puis une seconde en C. Ce document recense tous les fichiers du projet ainsi que les fonctionnalités implémentées et leur utilisation.

## Partie Haskell

### Fichiers

- [AES.hs](haskell/AES.hs) : contient les fonctions d'encodage et de décodage pour des chaines de caractères d'une taille indéterminée. Peut s'exécuter comme un binaire après compilation.
- [Cipher.hs](haskell/Cipher.hs) : contient les fonctions de chiffrement et de déchiffrement d'un bloc de 16 octets ainsi que toutes les fonctions auxiliaires nécessaires (ex: addRoundKey, shiftRows, etc).
- [InvCipher.hs](haskell/InvCipher.hs) : contient les fonctions de déchiffrement d'un bloc de 16 octets ainsi que toutes les fonctions auxiliaires nécessaires.
- [Math_preliminaries.hs](haskell/Math_preliminaries.hs) : contient les fonctions mathématiques principales utilisées par nos types de données (ex: addition, multiplication, etc)
- [Structure_algebrique.hs](haskell/Structure_algebrique.hs) : contient les définitions de classes d'équivalence pour nos types.
- [test_aes.ghci](haskell/test_unitaires/test_aes.ghci) : fichier de test pour les fonctions de [AES.hs](haskell/AES.hs).
- [test_cipher.ghci](haskell/test_unitaires/test_cipher.ghci) : fichier de test pour les fonctions de [Cipher.hs](haskell/Cipher.hs) et de [InvCipher.hs](haskell/InvCipher.hs).
- [test_math_preliminaries.ghci](haskell/test_unitaires/test_math_preliminaries.ghci) : fichier de test pour les fonctions de [Math_preliminaries.hs](haskell/Math_preliminaries.hs).
- [test_structure_algebrique.ghci](haskell/test_unitaires/test_structure_algebrique.ghci) : fichier de test pour les fonctions de [Structure_algebrique.hs](haskell/Structure_algebrique.hs).

### Compilation

Pour compiler les fichiers, il suffit de se placer dans le dossier [haskell](haskell) et d'exécuter la commande suivante :

```bash
ghc --make AES.hs -o ./AES
```

### Utilisation

Pour utiliser le programme, on peut passer directement par l'intépréteur de commande de Haskell en exécutant les commandes suivantes :

```bash
ghci
ghci> :l AES.hs
```

On peut alors utiliser le binaire [AES](haskell/AES) en utilisant la syntaxe suivante :

```bash
./AES encode
./AES decode
./AES -e <key> <msg>
./AES -d <key> <msg>
```

## Partie C

### Fichiers
- [aes.c](c/aes.c) : contient les fonctions d'encodage et de décodage pour des chaines de caractères d'une taille indéterminée (multiple de 16).
- [aes.h](c/aes.h) : contient les prototypes des fonctions de [aes.c](c/aes.c).
- [bitmap.c](c/bitmap.c) : contient les fonctions de manipulation de fichiers bitmap (chiffrement, déchiffrement, etc).
- [bitmap.h](c/bitmap.h) : contient les prototypes des fonctions de [bitmap.c](c/bitmap.c).
- [cipher.c](c/cipher.c) : contient les fonctions de chiffrement et de déchiffrement d'un bloc de 16 octets ainsi que toutes les fonctions auxiliaires nécessaires (ex: addRoundKey, shiftRows, etc).
- [cipher.h](c/cipher.h) : contient les prototypes des fonctions de [cipher.c](c/cipher.c).
- [entropie.c](c/entropie.c) : contient les fonctions de calcul d'entropie d'un fichier.
- [entropie.h](c/entropie.h) : contient les prototypes des fonctions de [entropie.c](c/entropie.c).
- [Makefile](c/Makefile) : contient les règles de compilation pour les fichiers C.
- [tests.c](c/tests.c) : contient des fonctions de test pour toutes les fonctions de [aes.c](c/aes.c), [bitmap.c](c/bitmap.c), [cipher.c](c/cipher.c) et [tools.c](c/tools.c).
- [tests.h](c/tests.h) : contient les prototypes des fonctions de [tests.c](c/tests.c).
- [tools.c](c/tools.c) : contient des fonctions annexes utilisées par les autres fichiers (ex: mutiplication, affichage d'un tableau, etc).
- [tools.h](c/tools.h) : contient les prototypes des fonctions de [tools.c](c/tools.c).
- [file1.txt](c/txt_files/file1.txt) : fichier texte utilisé pour les tests (léger, 128O).
- [expected1.txt](c/txt_files/expected1.txt) : fichier texte contenant le résultat attendu après le chiffrement de [file1.txt](c/txt_files/file1.txt).
- [sample.txt](c/txt_files/sample.txt) : fichier texte utilisé pour les tests (gros, 1.26Go, généré avec openssl).
- [bitmap_file.bmp](c/bitmap_files/bitmap_file.bmp) : fichier bitmap utilisé pour les tests (léger, 799Ko).
- [bitmap_file_2.bmp](c/bitmap_files/bitmap_file_2.bmp) : fichier bitmap identique à [bitmap_file.bmp](c/bitmap_files/bitmap_file.bmp) mais plus gros (52Mo).
- [bitmap_original.bmp](c/bitmap_files/bitmap_original.bmp) : fichier bitmap utilisé pour les tests (intermédiaire, 2.9Mo).

### Compilation et Utilisation

Pour compiler les fichiers, il suffit de se placer dans le dossier [c](c) et d'exécuter la commande suivante :

```bash
make
```

Si on veut chiffrement une image bmp, il faut exécuter les commandes suivante :

```bash
make bitmap
./bitmap <input_file>
```

Si on veut compiler le fichier de test, il suffit d'exécuter la commande suivante :

```bash
make tests
```

On peut régler certains paramètres lors de la compilation comme l'utilisation d'un autre compilateur, le fait d'activer l'optimisation, etc. À titre d'exemple, on pourrait fournir la commande suivante :

```bash
make bitmap COMPIL=gcc OPTMIZE=yes CALLGRIND=no
```

Il est également possible de supprimer les fichiers générés lors de la compilation en exécutant la commande suivante :

```bash
make clean
```

Enfin, si on souhaite utiliser `kcachegrind` pour visualiser les résultats de `valgrind`, il faut exécuter les commandes suivante :

```bash
make callgrind CALLGRIND=yes
kcachegrind callgrind.out
```

## Annexes
- Documentation AES : PDF de documentation qui résume le document [nist.fips.197.pdf](pdf/nist.fips.197.pdf).
- [nist.fips.197.pdf](pdf/nist.fips.197.pdf) : document officiel du NIST décrivant l'algorithme AES.
- [px222_1.pdf](pdf/px222_1.pdf) : 1er document de présentation et d'explication du projet.
- [px222_2.pdf](pdf/px222_2.pdf) : Suite de la présentation et l'explication du projet (précision sur les nouvelles fonctionnalités).
- [sheet.pdf](pdf/sheet.pdf) : Documentation de présentation de l'outil Git.
- [carnet_de_bord.md](carnet_de_bord.md) : Carnet de bord du projet.
- [first_kcachegrind.png](img_CdB/first_kcachegrind.png) : résultat de l'utilisation de `kcachegrind` sur notre code sans optimisation.
- [opti_multi_kcachegrind.png](img_CdB/opti_multi_kcachegrind.png) : résultat de l'utilisation de `kcachegrind` sur notre code avec optimisation (au niveau de la fonction mixColumns présente dans [cipher.c](c/cipher.c)).
- [fonctionnalités_et_choix_techniques.md](fonctionnalités_et_choix_techniques.md) : Document de présentation des fonctionnalités et des choix techniques au niveau du 1er jalon (implémentation en Haskell).
- [doc_code_c.pdf](doc_code_c.pdf) : Documentation du code C généré par Doxygen grâce au format de commentaire utilisé tout au long du projet.
- [doc_doxygen](doc_doxygen) : Dossier contenant les fichiers générés par Doxygen.
- [.gitignore](.gitignore) : Fichier de configuration de Git pour ignorer certains fichiers lors des commits.
- [README.md](README.md) : Fichier de présentation du projet.