module Math_preliminaries (addition, multiplication) where

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

-- newtype GF = Gf [Z_sur_2Z] deriving (Show)

p1_ex = [Z2Z 1, Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 0, Z2Z 1] -- 1 + x + x² + x⁴ + x⁶
p2_ex = [Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 1] -- 1 + x + x⁷

res_add = [Z2Z 0, Z2Z 0, Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 1] -- x² + x⁴ + x⁶ + x⁷
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
-----------------------------------------------------------------

poly_irreductible = [Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 1, Z2Z 0, Z2Z 0, Z2Z 0, Z2Z 1] -- 1 + x + x³ + x⁴ + x⁸ (ordre)

res_multi = [Z2Z 1,Z2Z 0,Z2Z 0,Z2Z 0,Z2Z 0,Z2Z 0,Z2Z 1,Z2Z 1] -- 1 + x⁶ + x⁷
-----------------------------------------------------------------
------------------------ Multiplication -------------------------
-----------------------------------------------------------------
multiplication :: [Z_sur_2Z] -> [Z_sur_2Z] -> [Z_sur_2Z]
multiplication nr mr = poly_mod (multi_aux nr mr) poly_irreductible

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

poly_mod :: [Z_sur_2Z] -> [Z_sur_2Z] -> [Z_sur_2Z]
poly_mod res irr | (length res - 1) > (length irr - 1) =  poly_mod (divise res irr q) irr
                 | otherwise = res
                 where q = create_poly ((length res - 1) - (length irr - 1))

create_poly :: Int -> [Z_sur_2Z]
create_poly 0 = [(Z2Z 1)]
create_poly n = (Z2Z 0) : (create_poly (n-1))

divise :: [Z_sur_2Z] -> [Z_sur_2Z] -> [Z_sur_2Z] -> [Z_sur_2Z]
divise p irr q = down_degree (addition p (multi_aux irr q))

down_degree :: [Z_sur_2Z] -> [Z_sur_2Z]
down_degree p = reverse (cut_poly (reverse p))

cut_poly :: [Z_sur_2Z] -> [Z_sur_2Z]
cut_poly (p:pr) | p == (Z2Z 0) = cut_poly pr
                | otherwise = (p:pr)
-----------------------------------------------------------------