{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

-- module Math_preliminaries (Poly, toPoly, addition, multiplication, pIrr4, pIrr8) where
module Math_preliminaries where

import Structure_algebrique


-----------------------------------------------------------------
--------------------------- Types -------------------------------
-----------------------------------------------------------------
newtype Z_sur_2Z = Z2Z Integer deriving (Show, Eq)
newtype Poly a = Pol [a] deriving (Show, Eq)
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- Constantes ----------------------------
-----------------------------------------------------------------
pIrr8 = toPoly [1, 1, 0, 1, 1, 0, 0, 0, 1] -- 1 + x + x³ + x⁴ + x⁸ (ordre)
pIrr4 = toPoly [1, 0, 0, 0, 1] -- 1 + x⁴


toPoly :: [Integer] -> Poly Z_sur_2Z
toPoly xs = Pol $ map (toZ2Z . Z2Z) xs
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

toZ2Z :: Z_sur_2Z -> Z_sur_2Z
toZ2Z (Z2Z n) = Z2Z $ n `mod` 2
-----------------------------------------------------------------



-----------------------------------------------------------------
-------------------------- Instance -----------------------------
-----------------------------------------------------------------
instance Groupe Z_sur_2Z where
      neutre = Z2Z 0
      operation = xor
      inverse = oppose2
      conversion = toZ2Z

instance Anneau Z_sur_2Z where
      -- multiplication = 

instance Corps Z_sur_2Z where
      -- inverseMultiplicatif = 

instance Corps a => Groupe (Poly a) where
      -- neutre = Pol [1]
      operation = addition_poly
      -- inverse = 
      conversion = toPoly
  
instance Corps a => Anneau (Poly a) where
      multiplication = multiplication_poly
-----------------------------------------------------------------



-----------------------------------------------------------------
-------------------------- Addition -----------------------------
-----------------------------------------------------------------
-- Notation : Addition -> + entouré

addition_poly :: Poly Z_sur_2Z -> Poly Z_sur_2Z -> Poly Z_sur_2Z
addition_poly (Pol []) (Pol []) = Pol []
addition_poly (Pol nr) (Pol []) = Pol nr
addition_poly (Pol []) (Pol mr) = Pol mr
addition_poly (Pol (n:nr)) (Pol (m:mr)) = Pol (x:r)
                                 where x = xor (toZ2Z n) (toZ2Z m)
                                       (Pol r) = addition_poly (Pol nr) (Pol mr)
-----------------------------------------------------------------


-----------------------------------------------------------------
------------------------ Multiplication -------------------------
-----------------------------------------------------------------
-- Notation : Multiplication -> •

multiplication_poly :: Poly Z_sur_2Z -> Poly Z_sur_2Z -> Poly Z_sur_2Z -> Poly Z_sur_2Z
multiplication_poly (Pol nr) (Pol mr) (Pol p_irr) = poly_mod (multi_aux (Pol nr) (Pol mr)) (Pol p_irr)

multi_aux :: Poly Z_sur_2Z -> Poly Z_sur_2Z -> Poly Z_sur_2Z
multi_aux (Pol []) _ = Pol []
multi_aux (Pol (n:nr)) (Pol mr) = addition (multi_aux2 n (Pol mr)) (Pol (x:r))
                              where x = Z2Z 0
                                    (Pol r) = multi_aux (Pol nr) (Pol mr)

multi_aux2 :: Z_sur_2Z -> Poly Z_sur_2Z -> Poly Z_sur_2Z
multi_aux2 _ (Pol []) = Pol []
multi_aux2 n (Pol (m:mr)) = Pol (x:r)
                          where x = multi (toZ2Z n) (toZ2Z m)
                                (Pol r) = multi_aux2 n (Pol mr)

multi :: Z_sur_2Z -> Z_sur_2Z -> Z_sur_2Z
multi _ (Z2Z 0) = Z2Z 0
multi (Z2Z 0) _ = Z2Z 0
multi (Z2Z 1) (Z2Z 1) = Z2Z 1

poly_mod :: Poly Z_sur_2Z -> Poly Z_sur_2Z -> Poly Z_sur_2Z
poly_mod (Pol res) (Pol irr) | (length res - 1) >= (length irr - 1) =  poly_mod (divise (Pol res) (Pol irr) q) (Pol irr)
                             | otherwise = (Pol res)
                             where q = create_poly ((length res - 1) - (length irr - 1))

create_poly :: Int -> Poly Z_sur_2Z
create_poly 0 = Pol [(Z2Z 1)]
create_poly n = Pol (x:r)
                where x = Z2Z 0
                      (Pol r) = create_poly (n-1)

divise :: Poly Z_sur_2Z -> Poly Z_sur_2Z -> Poly Z_sur_2Z -> Poly Z_sur_2Z
divise (Pol p) (Pol irr) (Pol q) = down_degree (addition (Pol p) (multi_aux (Pol irr) (Pol q)))

down_degree :: Poly Z_sur_2Z -> Poly Z_sur_2Z
down_degree (Pol p) = revPol (cut_poly (revPol (Pol p)))

revPol :: Poly Z_sur_2Z -> Poly Z_sur_2Z
revPol (Pol p) = Pol $ reverse p

cut_poly :: Poly Z_sur_2Z -> Poly Z_sur_2Z
cut_poly (Pol (p:pr)) | p == (Z2Z 0) = (cut_poly (Pol pr))
                      | otherwise = Pol $ (p:pr)
-----------------------------------------------------------------

-----------------------------------------------------------------
--------------------------- Inverse -----------------------------
-----------------------------------------------------------------
-- (1 + x² + x³ + x⁷) * x = 1 mod (1 + x + x³ + x⁴ + x⁸)
-- [1, 0, 1, 1, 0, 0, 0, 1] * [0, 1] = [1]
-- 141 * 2 = 282 = 1 [281]    -- modulo 281 car (pToDec pIrr8 - 1)

-- pInv :: Poly -> Poly

pToDec :: Poly Z_sur_2Z -> Integer
pToDec p = pToDec_aux p 0

pToDec_aux :: Poly Z_sur_2Z -> Integer -> Integer
pToDec_aux (Pol []) _ = 0
pToDec_aux (Pol ((Z2Z x):xs)) i = x*(2^i) + (pToDec_aux (Pol xs) (i+1))

decToP :: Integer -> Poly Z_sur_2Z
decToP n = down_degree $ revPol $ decToP_aux n 7

-- pb avec les polys qui ne commence pas pas 1 (ex: x + x²)
decToP_aux :: Integer -> Integer -> Poly Z_sur_2Z
decToP_aux n i | n - (2^i) > 0 = Pol (x:r)
               | n - (2^i) == 0 = Pol [Z2Z 1]
            --    | i <= 0 = Pol [Z2Z 0]
               | otherwise = Pol (y:v)
               where x = Z2Z 1
                     y = Z2Z 0
                     (Pol r) = decToP_aux (n - (2^i)) (i-1)
                     (Pol v) = decToP_aux n (i-1)
-----------------------------------------------------------------
