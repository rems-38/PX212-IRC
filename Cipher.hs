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

cipher_key_ex :: Block
cipher_key_ex = toBlock ["2b", "7e", "15", "16", "28", "ae", "d2", "a6", "ab", "f7", "15", "88", "09", "cf", "4f", "3c"]

input_appendixB :: Block
input_appendixB = toBlock ["32", "43", "f6", "a8", "88", "5a", "30", "8d", "31", "31", "98", "a2", "e0", "37", "07", "34"]

result :: Block
result = toBlock ["19", "3d", "e3", "be", "a0", "f4", "e2", "2b", "9a", "c6", "8d", "2a", "e9", "f8", "48", "08"]
-----------------------------------------------------------------


-----------------------------------------------------------------
------------------------- Fonctions -----------------------------
-----------------------------------------------------------------
addRoundKey :: Block -> Block -> Block
addRoundKey b1 b2 = map down_degree (addRoundKey_aux b1 b2)

addRoundKey_aux :: Block -> Block -> Block
addRoundKey_aux = zipWith operation
-----------------------------------------------------------------