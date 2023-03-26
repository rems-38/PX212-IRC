{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Math_preliminaries (GF, addition, multiplication) where

import Group
import ZsurNZ

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

newtype GF = Gf [Z_sur_2Z] deriving (Show, Eq)

toGF :: [Integer] -> GF
toGF xs = Gf $ map (toZ2Z . Z2Z) xs

p1_ex = toGF [1, 1, 1, 0, 1, 0, 1] -- 1 + x + x² + x⁴ + x⁶
p2_ex = toGF [1, 1, 0, 0, 0, 0, 0, 1] -- 1 + x + x⁷

res_add = toGF [0, 0, 1, 0, 1, 0, 1, 1] -- x² + x⁴ + x⁶ + x⁷
-----------------------------------------------------------------
-------------------------- Addition -----------------------------
-----------------------------------------------------------------
-- Notation : Addition -> + entouré

xor :: Z_sur_2Z -> Z_sur_2Z -> Z_sur_2Z
xor a b | a == b = Z2Z 0
        | a /= b = Z2Z 1

addition :: GF -> GF -> GF
addition (Gf []) (Gf []) = Gf []
addition (Gf nr) (Gf []) = Gf nr
addition (Gf []) (Gf mr) = Gf mr
addition (Gf (n:nr)) (Gf (m:mr)) = Gf (x:r)
                                 where x = xor (toZ2Z n) (toZ2Z m)
                                       (Gf r) = addition (Gf nr) (Gf mr)
-----------------------------------------------------------------

poly_irreductible = toGF [1, 1, 0, 1, 1, 0, 0, 0, 1] -- 1 + x + x³ + x⁴ + x⁸ (ordre)
res_multi = toGF [1, 0, 0, 0, 0, 0, 1, 1] -- 1 + x⁶ + x⁷

-----------------------------------------------------------------
------------------------ Multiplication -------------------------
-----------------------------------------------------------------
-- Notation : Multiplication -> •

multiplication :: GF -> GF -> GF
multiplication (Gf nr) (Gf mr) = poly_mod (multi_aux (Gf nr) (Gf mr)) poly_irreductible

multi_aux :: GF -> GF -> GF
multi_aux (Gf []) _ = Gf []
multi_aux (Gf (n:nr)) (Gf mr) = addition (multi_aux2 n (Gf mr)) (Gf (x:r))
                              where x = Z2Z 0
                                    (Gf r) = multi_aux (Gf nr) (Gf mr)

multi_aux2 :: Z_sur_2Z -> GF -> GF
multi_aux2 _ (Gf []) = Gf []
multi_aux2 n (Gf (m:mr)) = Gf (x:r)
                          where x = multi (toZ2Z n) (toZ2Z m)
                                (Gf r) = multi_aux2 n (Gf mr)

multi :: Z_sur_2Z -> Z_sur_2Z -> Z_sur_2Z
multi _ (Z2Z 0) = Z2Z 0
multi (Z2Z 0) _ = Z2Z 0
multi (Z2Z 1) (Z2Z 1) = Z2Z 1

poly_mod :: GF -> GF -> GF
poly_mod (Gf res) (Gf irr) | (length res - 1) >= (length irr - 1) =  poly_mod (divise (Gf res) (Gf irr) q) (Gf irr)
                 | otherwise = (Gf res)
                 where q = create_poly ((length res - 1) - (length irr - 1))

create_poly :: Int -> GF
create_poly 0 = Gf [(Z2Z 1)]
create_poly n = Gf (x:r)
                where x = Z2Z 0
                      (Gf r) = create_poly (n-1)

divise :: GF -> GF -> GF -> GF
divise (Gf p) (Gf irr) (Gf q) = down_degree (addition (Gf p) (multi_aux (Gf irr) (Gf q)))

down_degree :: GF -> GF
down_degree (Gf p) = revGf (cut_poly (revGf (Gf p)))

revGf :: GF -> GF
revGf (Gf p) = Gf $ reverse p

cut_poly :: GF -> GF
cut_poly (Gf (p:pr)) | p == (Z2Z 0) = (cut_poly (Gf pr))
                     | otherwise = Gf $ (p:pr)
-----------------------------------------------------------------