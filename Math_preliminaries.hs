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

poly_irreductible = [1, 1, 0, 1, 1, 0, 0, 0, 1] -- 1 + x + x³ + x⁴ + x⁸ (ordre)

-----------------------------------------------------------------
------------------------ Multiplication -------------------------
-----------------------------------------------------------------
-- multiplication :: [Integer] -> [Integer] -> [Integer]
-- multiplication 


multi_aux :: [Z_sur_2Z] -> [Z_sur_2Z] -> [Z_sur_2Z]
multi_aux [] _ = []
multi_aux (n:nr) mr = addition (multi_aux2 n mr) ((Z2Z 0) : (multi_aux nr mr))


multi_aux2 :: Z_sur_2Z -> [Z_sur_2Z] -> [Z_sur_2Z]
multi_aux2 _ [] = []
multi_aux2 n (m:mr) = (multi (toZ2Z n) (toZ2Z m)) : (multi_aux2 n mr)

multi :: Z_sur_2Z -> Z_sur_2Z -> Z_sur_2Z
multi _ (Z2Z 0) = (Z2Z 0)
multi (Z2Z 0) _ = (Z2Z 0)
multi (Z2Z 1) (Z2Z 1) = (Z2Z 1)


-----------------------------------------------------------------