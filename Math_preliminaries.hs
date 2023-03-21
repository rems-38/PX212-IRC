module Math_preliminaries where

import Group
import ZsurNZ

-- tips Euclide Etendu pour inverse des polynomes

-----------------------------------------------------------------
-- Définition de Z sur 2Z, instanciation dans la classe groupe --
-----------------------------------------------------------------
newtype Z_sur_2Z = Z2Z Integer deriving (Show, Eq)

addMod2 :: Z_sur_2Z-> Z_sur_2Z -> Z_sur_2Z
addMod2 (Z2Z a) (Z2Z b) = Z2Z $ addModP 2 a b

oppose2 :: Z_sur_2Z -> Z_sur_2Z
oppose2 (Z2Z n) = Z2Z $ oppose 2 n

toZ2Z :: Z_sur_2Z -> Z_sur_2Z
toZ2Z (Z2Z n) = Z2Z $ n `mod` 2

instance Group Z_sur_2Z where
  unit = Z2Z 0
  inverse = oppose2
  operation = addMod2
-----------------------------------------------------------------

-- newtype Polynome = Poly [Z_sur_2Z] deriving (Show)

-----------------------------------------------------------------
-------------------------- Addition -----------------------------
-----------------------------------------------------------------
xor :: Z_sur_2Z -> Z_sur_2Z -> Z_sur_2Z
xor a b | a == b = (Z2Z 0)
        | a /= b = (Z2Z 1)

addition :: [Z_sur_2Z] -> [Z_sur_2Z] -> [Z_sur_2Z]
addition [] [] = []
addition nr [] = nr
addition [] mr = mr
addition (n:nr) (m:mr) = (xor (toZ2Z n) (toZ2Z m)) : (addition nr mr)

-- addition :: Polynome -> Polynome -> Polynome
-- addition (Poly []) (Poly []) = (Poly [])
-- addition (Poly (n:nr)) (Poly (m:mr)) = Poly $ (xor n m) : (addition (Poly nr) (Poly mr))
-----------------------------------------------------------------
