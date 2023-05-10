{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Cipher where

import Structure_algebrique
import Math_preliminaries


-----------------------------------------------------------------
--------------------------- Types -------------------------------
-----------------------------------------------------------------
type Byte = Poly Z_sur_2Z -- GF256
type Block = [Byte]
-----------------------------------------------------------------


-----------------------------------------------------------------
--------------------------- Cipher ------------------------------
-----------------------------------------------------------------
-- Squelette de la fonction de chiffrement
-- Surement à modifier (notamenet l'appel de word à chaque fois qui doit surement être réduit etc)
-- Regarder dans la doc comment déterminer le nombre de répétition
-- Je crois qu'il n'y a pas le cas final (en dehors de la boucle du coup (ie un tour sans mixColumns ?))
cipher :: Block -> Block -> Block
cipher input word = cipher_aux (addRoundKey input word) word 10

cipher_aux :: Block -> Block -> Int -> Block
cipher_aux input word n | n == 0 = input
                        | otherwise = cipher_aux (addRoundKey (mixColumns (shiftRows (subBytes input))) word) word (n-1)
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- Constantes ----------------------------
-----------------------------------------------------------------
toBlock :: String -> Block
toBlock s = map hexPol (words s)
-----------------------------------------------------------------


-----------------------------------------------------------------
------------------------ addRoundKey ----------------------------
-----------------------------------------------------------------
addRoundKey :: Block -> Block -> Block
addRoundKey b1 b2 = map down_degree (addRoundKey_aux b1 b2)

addRoundKey_aux :: Block -> Block -> Block
addRoundKey_aux = zipWith operation
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- subBytes ------------------------------
-----------------------------------------------------------------
subBytes :: Block -> Block
subBytes b = b
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- shiftRows -----------------------------
-----------------------------------------------------------------
shiftRows :: Block -> Block
shiftRows b = switchColRows (shiftRows_aux (switchColRows b) 0)

shiftRows_aux :: Block -> Int -> Block
shiftRows_aux b n | n == 4 = []
                  | otherwise = littleShift (take 4 b) n ++ shiftRows_aux (drop 4 b) (n+1)

-- Échange les colonnes et les lignes d'une matrice 4x4
-- Ex : [1..16] -> [1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 16]
switchColRows :: Block -> Block
switchColRows b = nthOfList b 1 ++ nthOfList b 2 ++ nthOfList b 3 ++ nthOfList b 4

-- D'après la visualisation en matrice 4x4, ça output la ligne n
-- Ex : [1, 2, 3, 4, 5, 6, 7, 8] 2 -> [2, 6, 10, 14]
nthOfList :: Block -> Int -> Block
nthOfList [] _ = []
nthOfList b 0 = []
nthOfList b n = head (drop (n-1) (take 4 b)) : nthOfList (drop 4 b) n

-- Rotation vers la gauche de n bits
-- Ex : [1, 2, 3, 4] 2 -> [3, 4, 1, 2]
littleShift :: Block -> Int -> Block
littleShift b n | n == 0 = b
                | otherwise = rb ++ lb
                where (lb, rb) = splitAt n b
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- mixColumns ----------------------------
-----------------------------------------------------------------
mixColumns :: Block -> Block
mixColumns b = b
-----------------------------------------------------------------