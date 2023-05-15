{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module InvCipher where

import Structure_algebrique
import Math_preliminaries
import Cipher


-----------------------------------------------------------------
------------------------- InvCipher -----------------------------
-----------------------------------------------------------------
invCipher :: Int -> Block -> Block -> Block
invCipher n input word | n == 128 = invCipher_aux (addRoundKey input word) (drop (4 * 4) (keyExpansion word 4 10)) 4 10
                       | n == 192 = invCipher_aux (addRoundKey input word) (drop (4 * 6) (keyExpansion word 6 12)) 6 12
                       | n == 256 = invCipher_aux (addRoundKey input word) (drop (4 * 8) (keyExpansion word 8 14)) 8 14
                       | otherwise = error "Chiffrements possible : AES-128, AES-192, AES-256"

invCipher_aux :: Block -> Block -> Int -> Int -> Block
invCipher_aux input word nk nr | nr == 1 = addRoundKey (invSubBytes $ invShiftRows input) (take (4 * nk) word)
                               | otherwise = invCipher_aux (invMixColumns (addRoundKey (invSubBytes $ invShiftRows input) (take (4 * nk) word))) (drop (4 * nk) word) nk (nr - 1)
-----------------------------------------------------------------



-----------------------------------------------------------------
-------------------------- invSubByes ---------------------------
-----------------------------------------------------------------
invSubBytes :: Block -> Block
invSubBytes b = b
-----------------------------------------------------------------



-----------------------------------------------------------------
-------------------------- invShiftRows --------------------------
-----------------------------------------------------------------
invShiftRows :: Block -> Block
invShiftRows b = b
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- invMixColumns -------------------------
-----------------------------------------------------------------
invMixColumns :: Block -> Block
invMixColumns b = b
-----------------------------------------------------------------