{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- module Math_preliminaries (Poly, toPoly_Z2Z, addition, multiplication, pIrr4, pIrr8) where
module Math_preliminaries where

import Structure_algebrique
import Data.Typeable

-----------------------------------------------------------------
------------------------- Constantes ----------------------------
-----------------------------------------------------------------
-- pIrr8 = toPoly_Z2Z [1, 1, 0, 1, 1, 0, 0, 0, 1] -- 1 + x + x³ + x⁴ + x⁸ (ordre)
-- pIrr4 = toPoly_Z2Z [1, 0, 0, 0, 1] -- 1 + x⁴


toPoly_Z2Z :: [Integer] -> Poly Z_sur_2Z
toPoly_Z2Z xs = Pol $ map (toZ2Z . Z2Z) xs
-----------------------------------------------------------------



-----------------------------------------------------------------
----------------------- Opérations Z/2Z -------------------------
-----------------------------------------------------------------
xor :: Z_sur_2Z -> Z_sur_2Z -> Z_sur_2Z
xor a b | a == b = Z2Z 0
        | a /= b = Z2Z 1

oppose :: Integer -> Integer -> Integer
oppose p n | n == 0    = 0
           | otherwise = p - n

oppose2 :: Z_sur_2Z -> Z_sur_2Z
oppose2 (Z2Z n) = Z2Z $ oppose 2 n

multi2 :: Z_sur_2Z -> Z_sur_2Z -> Z_sur_2Z
multi2 _ (Z2Z 0) = Z2Z 0
multi2 (Z2Z 0) _ = Z2Z 0
multi2 (Z2Z 1) (Z2Z 1) = Z2Z 1

inv2 :: Z_sur_2Z -> Maybe Z_sur_2Z
inv2 (Z2Z 0) = Nothing
inv2 (Z2Z 1) = Just $ Z2Z 1

toZ2Z :: Z_sur_2Z -> Z_sur_2Z
toZ2Z (Z2Z n) = Z2Z $ n `mod` 2
-----------------------------------------------------------------



-----------------------------------------------------------------
-------------------------- Instance -----------------------------
-----------------------------------------------------------------
instance Groupe Z_sur_2Z where
      neutre = Z2Z 0
      unite = Z2Z 1
      operation = xor
      inverse = oppose2

instance Anneau Z_sur_2Z where
      multiplication = multi2

instance Corps Z_sur_2Z where
      inverseMultiplicatif = inv2

instance Corps a => Groupe (Poly a) where
      neutre = Pol [neutre] 
      unite = Pol [unite]
      operation = addition_poly
      -- inverse = 
  
-- instance Corps a => Anneau (Poly a) where
--       multiplication = multiplication_poly

instance IrreduciblePoly Z_sur_2Z where
      polyIrr = toPoly_Z2Z [1, 1, 0, 1, 1, 0, 0, 0, 1]
-----------------------------------------------------------------



-----------------------------------------------------------------
-------------------------- Addition -----------------------------
-----------------------------------------------------------------
-- Notation : Addition -> + entouré

addition_poly :: Corps a => Poly a -> Poly a -> Poly a
addition_poly (Pol []) (Pol []) = Pol []
addition_poly (Pol nr) (Pol []) = Pol nr
addition_poly (Pol []) (Pol mr) = Pol mr
addition_poly (Pol (n:nr)) (Pol (m:mr)) = Pol (x:r)
                                          where x = operation n m
                                                (Pol r) = addition_poly (Pol nr) (Pol mr)
-----------------------------------------------------------------


-----------------------------------------------------------------
------------------------ Multiplication -------------------------
-----------------------------------------------------------------
-- Notation : Multiplication -> •

multiplication_poly :: (IrreduciblePoly a, Eq a, Corps a) => Poly a -> Poly a -> Poly a
multiplication_poly (Pol nr) (Pol mr) = poly_mod (multi_aux (Pol nr) (Pol mr)) polyIrr

multi_aux :: Corps a => Poly a -> Poly a -> Poly a
multi_aux (Pol []) _ = Pol []
multi_aux (Pol (n:nr)) (Pol mr) = operation (multi_aux2 n (Pol mr)) (Pol (neutre:r))
                                  where (Pol r) = multi_aux (Pol nr) (Pol mr)

multi_aux2 :: Corps a => a -> Poly a -> Poly a
multi_aux2 _ (Pol []) = Pol []
multi_aux2 n (Pol (m:mr)) = Pol (x:r)
                          where x = multiplication n m
                                (Pol r) = multi_aux2 n (Pol mr)

poly_mod :: (Eq a, Corps a) => Poly a -> Poly a -> Poly a
poly_mod (Pol res) (Pol irr) | (length res - 1) >= (length irr - 1) =  poly_mod (divise (Pol res) (Pol irr) q) (Pol irr)
                             | otherwise = (Pol res)
                             where q = create_poly ((length res - 1) - (length irr - 1))

create_poly :: Corps a => Int -> Poly a
create_poly 0 = Pol [unite]
create_poly n = Pol (x:r)
                where x = neutre
                      (Pol r) = create_poly (n-1)

divise :: (Eq a, Corps a) => Poly a -> Poly a -> Poly a -> Poly a
divise (Pol p) (Pol irr) (Pol q) = down_degree (operation (Pol p) (multi_aux (Pol irr) (Pol q)))

down_degree :: (Eq a, Corps a) => Poly a -> Poly a
down_degree (Pol p) = revPol (cut_poly (revPol (Pol p)))

revPol :: Corps a => Poly a -> Poly a
revPol (Pol p) = Pol $ reverse p

cut_poly :: (Eq a, Corps a) => Poly a -> Poly a
cut_poly (Pol (p:pr)) | p == neutre = (cut_poly (Pol pr))
                      | otherwise = Pol $ (p:pr)
-----------------------------------------------------------------

-----------------------------------------------------------------
--------------------------- Inverse -----------------------------
-----------------------------------------------------------------
-- (1 + x² + x³ + x⁷) * x = 1 mod (1 + x + x³ + x⁴ + x⁸)
-- [1, 0, 1, 1, 0, 0, 0, 1] * [0, 1] = [1]
-- 141 * 2 = 282 = 1 [281]    -- modulo 281 car (pToDec pIrr8 - 1)

-- pInv :: Poly -> Poly

-- pToDec :: Poly Z_sur_2Z -> Integer
-- pToDec p = pToDec_aux p 0

-- pToDec_aux :: Poly Z_sur_2Z -> Integer -> Integer
-- pToDec_aux (Pol []) _ = 0
-- pToDec_aux (Pol ((Z2Z x):xs)) i = x*(2^i) + (pToDec_aux (Pol xs) (i+1))

-- decToP :: Integer -> Poly Z_sur_2Z
-- decToP n = down_degree $ revPol $ decToP_aux n 7

-- -- pb avec les polys qui ne commence pas pas 1 (ex: x + x²)
-- decToP_aux :: Integer -> Integer -> Poly Z_sur_2Z
-- decToP_aux n i | n - (2^i) > 0 = Pol (x:r)
--                | n - (2^i) == 0 = Pol [Z2Z 1]
--             --    | i <= 0 = Pol [Z2Z 0]
--                | otherwise = Pol (y:v)
--                where x = Z2Z 1
--                      y = Z2Z 0
--                      (Pol r) = decToP_aux (n - (2^i)) (i-1)
--                      (Pol v) = decToP_aux n (i-1)
-----------------------------------------------------------------
