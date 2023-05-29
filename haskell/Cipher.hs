{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE FlexibleInstances #-}

module Cipher where

import Structure_algebrique
import Math_preliminaries
import Data.Char


-----------------------------------------------------------------
------------------------------ NB -------------------------------
-----------------------------------------------------------------
--         Nk Nb Nr
-- AES-128  4  4 10 
-- AES-192  6  4 12 
-- AES-256  8  4 14
-----------------------------------------------------------------



-----------------------------------------------------------------
----------------------------- Types -----------------------------
-----------------------------------------------------------------
type Byte = Poly Z_sur_2Z -- GF256
type Block = [Byte]
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- Instanciation -------------------------
-----------------------------------------------------------------
instance Corps Byte where
    inverseMultiplicatif = inverse_poly
-----------------------------------------------------------------



-----------------------------------------------------------------
---------------------------- Parser -----------------------------
-----------------------------------------------------------------
-- Parser : hexa (plusieurs) -> liste de polynômes
toBlock :: String -> Block
toBlock s = map hexPol (words s)

blockStr :: Block -> String
blockStr b = init $ myConcat (map polHex b) -- init enlève le dernier espace
           where myConcat [] = ""
                 myConcat (x:xs) = x ++ " " ++ myConcat xs

-- Parser : hexa -> polynôme (juste un seul poly donc "5e" (pas de "ae b3"))
hexPol :: String -> Poly Z_sur_2Z
hexPol xs = down_degree $ revPol (foldr (mergePoly . aux) (Pol []) xs)
          where aux c | length tab == 1 = toPoly_Z2Z ([0, 0, 0] ++ tab)
                      | length tab == 2 = toPoly_Z2Z ([0, 0] ++ tab)
                      | length tab == 3 = toPoly_Z2Z (0 : tab)
                      | otherwise = toPoly_Z2Z tab
                      where tab = decToBin $ charInt c
-- peut etre faire hexPol [x,y] (car string 2 élements et du coup ça enlève des fonctions)

-- Parser : polynôme -> héxa (juste un seul poly donc "5e" (pas de "ae b3"))
polHex :: Byte -> String
polHex (Pol [-1, -1, -1, -1, -1, -1, -1, -1]) = ""
polHex p = case lp of
              [0, 0, 0, 0] -> (intChar . binToDec) (z2ZToInt $ reverse (up_degree4 rp)) : "0"
              _ -> (intChar . binToDec) (z2ZToInt $ reverse (up_degree4 rp)) : polHex (Pol ([-unite, -unite, -unite, -unite] ++ lp))
            where (lp, rp) = splitAt 4 (polArray p)

-- Parser : polynome -> liste (pour virer le constructeur)
polArray :: Poly a -> [a]
polArray (Pol xs) = xs

-- Parser : liste de Z2Z -> liste de int (pour virer le constructeur)
z2ZToInt :: [Z_sur_2Z] -> [Integer]
z2ZToInt [] = []
z2ZToInt ((Z2Z x):xs) = x : z2ZToInt xs

mergePoly :: Poly a -> Poly a -> Poly a
mergePoly (Pol []) (Pol []) = Pol []
mergePoly (Pol []) (Pol ys) = Pol ys
mergePoly (Pol xs) (Pol []) = Pol xs
mergePoly (Pol xs) (Pol ys) = Pol $ xs ++ ys

-- Transforme un char en entier (base hexadécimale)
charInt :: Char -> Integer
charInt 'a' = 10
charInt 'b' = 11
charInt 'c' = 12
charInt 'd' = 13
charInt 'e' = 14
charInt 'f' = 15
charInt c = fromIntegral $ digitToInt c

-- Transforme un entier en char (base hexadécimale)
intChar :: Integer -> Char
intChar 10 = 'a'
intChar 11 = 'b'
intChar 12 = 'c'
intChar 13 = 'd'
intChar 14 = 'e'
intChar 15 = 'f'
intChar n = chr (fromIntegral n + 48)

-- Transforme un entier en liste de 4 bits (bits de poids faible à droite)
-- Ex : 5 -> [1, 0, 1]
decToBin :: Integer -> [Integer]
decToBin 0 = [0]
decToBin n = reverse $ aux n
              where aux 0 = []
                    aux n = (n `mod` 2) : aux (n `div` 2)

-- Transforme une liste de 4 bits en entier (bits de poids faible à droite)
binToDec :: [Integer] -> Integer
binToDec list = aux list 3
              where aux [] _ = 0
                    aux (x:xs) n = (x*(2^n)) + aux xs (n-1)
-----------------------------------------------------------------



-----------------------------------------------------------------
--------------------------- Cipher ------------------------------
-----------------------------------------------------------------
cipher :: Int -> Block -> Block -> Block
cipher n input word | n == 128 = aux (addRoundKey input word) (drop 16 (keyExpansion word 4 10)) 10
                    | n == 192 = aux (addRoundKey input word) (drop 16 (keyExpansion word 6 12)) 12
                    | n == 256 = aux (addRoundKey input word) (drop 16 (keyExpansion word 8 14)) 14
                    | otherwise = error "Chiffrements possible : AES-128, AES-192, AES-256"
                    where aux input word nr | nr == 1 = addRoundKey (shiftRows $ subBytes input) (take 16 word)
                                            | otherwise = aux (addRoundKey (mixColumns $ shiftRows $ subBytes input) (take 16 word)) (drop 16 word) (nr - 1)
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- subBytes ------------------------------
-----------------------------------------------------------------
subBytes :: Block -> Block
subBytes b = subBytes_aux b sbox

subBytes_aux :: Block -> [[String]] -> Block
subBytes_aux [] _ = []
subBytes_aux (b:br) box = new_pol : subBytes_aux br box
                        where (lb, rb) = splitAt 4 (z2ZToInt (polArray (up_degree b)))
                              new_pol = hexPol $ box !! fromIntegral (binToDec (reverse lb)) !! fromIntegral (binToDec (reverse rb))

-- Sbox donnée dans le FIPS 197
sbox :: [[String]]
sbox = [["63", "ca", "b7", "04", "09", "53", "d0", "51", "cd", "60", "e0", "e7", "ba", "70", "e1", "8c"],
        ["7c", "82", "fd", "c7", "83", "d1", "ef", "a3", "0c", "81", "32", "c8", "78", "3e", "f8", "a1"],
        ["77", "c9", "93", "23", "2c", "00", "aa", "40", "13", "4f", "3a", "37", "25", "b5", "98", "89"],
        ["7b", "7d", "26", "c3", "1a", "ed", "fb", "8f", "ec", "dc", "0a", "6d", "2e", "66", "11", "0d"],
        ["f2", "fa", "36", "18", "1b", "20", "43", "92", "5f", "22", "49", "8d", "1c", "48", "69", "bf"],
        ["6b", "59", "3f", "96", "6e", "fc", "4d", "9d", "97", "2a", "06", "d5", "a6", "03", "d9", "e6"],
        ["6f", "47", "f7", "05", "5a", "b1", "33", "38", "44", "90", "24", "4e", "b4", "f6", "8e", "42"],
        ["c5", "f0", "cc", "9a", "a0", "5b", "85", "f5", "17", "88", "5c", "a9", "c6", "0e", "94", "68"],
        ["30", "ad", "34", "07", "52", "6a", "45", "bc", "c4", "46", "c2", "6c", "e8", "61", "9b", "41"],
        ["01", "d4", "a5", "12", "3b", "cb", "f9", "b6", "a7", "ee", "d3", "56", "dd", "35", "1e", "99"],
        ["67", "a2", "e5", "80", "d6", "be", "02", "da", "7e", "b8", "ac", "f4", "74", "57", "87", "2d"],
        ["2b", "af", "f1", "e2", "b3", "39", "7f", "21", "3d", "14", "62", "ea", "1f", "b9", "e9", "0f"],
        ["fe", "9c", "71", "eb", "29", "4a", "50", "10", "64", "de", "91", "65", "4b", "86", "ce", "b0"],
        ["d7", "a4", "d8", "27", "e3", "4c", "3c", "ff", "5d", "5e", "95", "7a", "bd", "c1", "55", "54"],
        ["ab", "72", "31", "b2", "2f", "58", "9f", "f3", "19", "0b", "e4", "ae", "8b", "1d", "28", "bb"],
        ["76", "c0", "15", "75", "84", "cf", "a8", "d2", "73", "db", "79", "08", "8a", "9e", "df", "16"]]
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- shiftRows -----------------------------
-----------------------------------------------------------------
shiftRows :: Block -> Block
shiftRows b = shiftRows_aux b 0 4 1

shiftRows_aux :: Block -> Int -> Int -> Int -> Block
shiftRows_aux b start end pas = switchColRows (aux (switchColRows b) start)
                              where aux b n | n == end = []
                                            | otherwise = littleShift (take 4 b) n ++ aux (drop 4 b) (n+pas)

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
mixColumns b = mixColumns_aux b a_x_mixColumns

mixColumns_aux :: Block -> Block -> Block
mixColumns_aux [] a_x = []
mixColumns_aux b a_x = colToNewPol (take 4 b) 4 a_x ++ mixColumns_aux (drop 4 b) a_x

colToNewPol :: Block -> Int -> Block -> Block
colToNewPol [] _ a_x = []
colToNewPol _ 0 a_x = []
colToNewPol b n a_x = somme (zipWith multiplication (littleShift a_x n) b) : colToNewPol b (n-1) a_x
                    where somme = foldr operation neutre

a_x_mixColumns :: Block
a_x_mixColumns = toBlock "02 03 01 01"
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
------------------------- keyExpansion --------------------------
-----------------------------------------------------------------
keyExpansion :: Block -> Int -> Int -> Block
keyExpansion key nk nr = expandWord key nk nr key

expandWord :: Block -> Int -> Int -> Block -> Block
expandWord key nk nr w | len == 4 * (nr + 1) = map down_degree w
                       | nk == 8 && len `mod` nk == 4 = funcRec (w ++ addRoundKey_aux (extract4 w (length w - (nk * 4))) (subWord (extract4 w (length w - 4)))) -- cas particulier pour AES-256
                       | len `mod` nk == 0 = funcRec (w ++ addRoundKey_aux (extract4 w (length w - (nk * 4))) (addRoundKey_aux afterOp pRcon))
                       | otherwise = funcRec (w ++ addRoundKey_aux extraction (extract4 w ((len - nk) * 4)))
                       where len = length w `div` 4
                             funcRec = expandWord key nk nr
                             pRcon = rcon (len `div` nk)
                             extraction = extract4 w (length w - 4)
                             afterOp = subWord $ rotWord extraction

extract4 :: Block -> Int -> Block
extract4 w n = take 4 (drop n w)

subWord :: Block -> Block
subWord = subBytes

rotWord :: Block -> Block
rotWord [w1, w2, w3, w4] = [w2, w3, w4, w1]

rcon :: Int -> Block
rcon i = poly_mod (create_poly (i-1)) polyIrr : toBlock "00 00 00"
-----------------------------------------------------------------