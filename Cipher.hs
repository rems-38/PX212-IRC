{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
import Structure_algebrique
import Math_preliminaries


-----------------------------------------------------------------
--------------------------- Types -------------------------------
-----------------------------------------------------------------
type Byte = Poly Z_sur_2Z -- GF256
type Block = [Byte]
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- Constantes ----------------------------
-----------------------------------------------------------------
toBlock :: [String] -> Block
toBlock = map hexPol

initTab :: [Int]
initTab = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]

after_shiftRows :: [Int]
after_shiftRows = [1,6,11,16,5,10,15,4,9,14,3,8,13,2,7,12]

-- shiftRows initTab == after_shiftRows
-- True (testé)
-----------------------------------------------------------------


-----------------------------------------------------------------
------------------------- Fonctions -----------------------------
-----------------------------------------------------------------
addRoundKey :: Block -> Block -> Block
addRoundKey b1 b2 = map down_degree (addRoundKey_aux b1 b2)

addRoundKey_aux :: Block -> Block -> Block
addRoundKey_aux = zipWith operation

shiftRows :: [Int] -> [Int]
shiftRows b = switchColRows (shiftRows_aux (switchColRows b) 0)

shiftRows_aux :: [Int] -> Int -> [Int]
shiftRows_aux b n | n == 4 = []
                  | otherwise = littleShift (take 4 b) n ++ shiftRows_aux (drop 4 b) (n+1)

-- Échange les colonnes et les lignes d'une matrice 4x4
-- Ex : [1..16] -> [1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 16]
switchColRows :: [Int] -> [Int]
switchColRows b = nthOfList b 1 ++ nthOfList b 2 ++ nthOfList b 3 ++ nthOfList b 4

-- D'après la visualisation en matrice 4x4, ça output la ligne n
-- Ex : [1, 2, 3, 4, 5, 6, 7, 8] 2 -> [2, 6, 10, 14]
nthOfList :: [Int] -> Int -> [Int]
nthOfList [] _ = []
nthOfList b 0 = []
nthOfList b n = head (drop (n-1) (take 4 b)) : nthOfList (drop 4 b) n

-- Rotation vers la gauche de n bits
-- Ex : [1, 2, 3, 4] 2 -> [3, 4, 1, 2]
littleShift :: [Int] -> Int -> [Int]
littleShift b n | n == 0 = b
                | otherwise = rb ++ lb
                where (lb, rb) = splitAt n b
-----------------------------------------------------------------