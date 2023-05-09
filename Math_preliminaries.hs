{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- module Math_preliminaries (Poly, toPoly_Z2Z, addition, multiplication, pIrr4, pIrr8) where
module Math_preliminaries where

import Structure_algebrique
import Data.Typeable ()
import Data.Data (Data)
import Data.Char


-----------------------------------------------------------------
-------------------------- NB/ToDo ------------------------------
-----------------------------------------------------------------
-- GF256 -> Poly Z_sur_2Z de degré <= 7
-- 
-- Inverse des polynômes :
-- presque fini, juste le calul du reste (coef) qui fail pour l'instant
--
-- Faire beaucoup de tests unitaires pour check les fonctions dans la partie des inverse
-----------------------------------------------------------------



-----------------------------------------------------------------
--------------------------- Types -------------------------------
-----------------------------------------------------------------
newtype Z_sur_2Z = Z2Z Integer deriving (Show, Eq, Num, Ord, Data)
newtype Poly a = Pol [a] deriving (Show, Eq)
-----------------------------------------------------------------



-----------------------------------------------------------------
-------------------------- Classes ------------------------------
-----------------------------------------------------------------
class (Corps a) => IrreduciblePoly a where
      polyIrr :: Poly a

class (Corps a) => Max a where
      nmax :: a -> Int
-----------------------------------------------------------------



-----------------------------------------------------------------
------------------------- Constantes ----------------------------
-----------------------------------------------------------------
toPoly_Z2Z :: [Integer] -> Poly Z_sur_2Z
toPoly_Z2Z xs = Pol $ map (toZ2Z . Z2Z) xs

hexPol :: String -> Poly Z_sur_2Z
hexPol xs = down_degree $ revPol (foldr (mergePoly . hexPol_aux) (Pol []) xs)

mergePoly :: Poly a -> Poly a -> Poly a
mergePoly (Pol []) (Pol []) = Pol []
mergePoly (Pol []) (Pol ys) = Pol ys
mergePoly (Pol xs) (Pol []) = Pol xs
mergePoly (Pol xs) (Pol ys) = Pol $ xs ++ ys

hexPol_aux :: Char -> Poly Z_sur_2Z
hexPol_aux c | length tab == 3 = toPoly_Z2Z (0 : tab)
             | otherwise = toPoly_Z2Z tab
             where tab = decToBinPol $ charInt c

charInt :: Char -> Integer
charInt 'a' = 10
charInt 'b' = 11
charInt 'c' = 12
charInt 'd' = 13
charInt 'e' = 14
charInt 'f' = 15
charInt c = fromIntegral $ digitToInt c

decToBinPol :: Integer -> [Integer]
decToBinPol 0 = [0]
decToBinPol n = reverse $ decToBin_aux n

decToBin_aux :: Integer -> [Integer]
decToBin_aux 0 = []
decToBin_aux n = (n `mod` 2) : decToBin_aux (n `div` 2)
-----------------------------------------------------------------



-----------------------------------------------------------------
----------------------- Instanciations --------------------------
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

instance (IrreduciblePoly a, Eq a, Corps a) => Groupe (Poly a) where
      neutre = Pol [neutre]
      unite = Pol [unite]
      operation = addition_poly
      inverse = inverse_poly

instance (IrreduciblePoly a, Eq a, Corps a) => Anneau (Poly a) where
      multiplication = multiplication_poly

instance IrreduciblePoly Z_sur_2Z where
      polyIrr = toPoly_Z2Z [1, 1, 0, 1, 1, 0, 0, 0, 1]
      -- pIrr8 = toPoly_Z2Z [1, 1, 0, 1, 1, 0, 0, 0, 1] -- 1 + x + x³ + x⁴ + x⁸ (ordre)
      -- pIrr4 = toPoly_Z2Z [1, 0, 0, 0, 1] -- 1 + x⁴

-- Instanciations de Z_sur_2Z dans Enum pour pouvoir utiliser fromEnum (dans pToDec_aux)
instance Enum Z_sur_2Z where
    fromEnum (Z2Z n) = fromEnum n
    toEnum n = Z2Z (toEnum n `mod` 2)
    -- /!\ NE PAS SE SERVIR DE toEnum (ça fais de la merde je crois)

instance Max Z_sur_2Z where
      nmax (Z2Z _) = 1
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

multi_aux :: (IrreduciblePoly a, Eq a, Corps a) => Poly a -> Poly a -> Poly a
multi_aux (Pol []) _ = Pol []
multi_aux (Pol (n:nr)) (Pol mr) = operation (multi_aux2 n (Pol mr)) (Pol (neutre:r))
                                  where (Pol r) = multi_aux (Pol nr) (Pol mr)

multi_aux2 :: Corps a => a -> Poly a -> Poly a
multi_aux2 _ (Pol []) = Pol []
multi_aux2 n (Pol (m:mr)) = Pol (x:r)
                          where x = multiplication n m
                                (Pol r) = multi_aux2 n (Pol mr)

poly_mod :: (IrreduciblePoly a, Eq a, Corps a) => Poly a -> Poly a -> Poly a
poly_mod (Pol res) (Pol irr) | (length res - 1) >= (length irr - 1) =  poly_mod (divise (Pol res) (Pol irr) q) (Pol irr)
                             | otherwise = Pol res
                             where q = create_poly ((length res - 1) - (length irr - 1))

create_poly :: Corps a => Int -> Poly a
create_poly 0 = Pol [unite]
create_poly n = Pol (x:r)
                where x = neutre
                      (Pol r) = create_poly (n-1)

divise :: (IrreduciblePoly a, Eq a, Corps a) => Poly a -> Poly a -> Poly a -> Poly a
divise (Pol p) (Pol irr) (Pol q) = down_degree (operation (Pol p) (multi_aux (Pol irr) (Pol q)))

down_degree :: (Eq a, Corps a) => Poly a -> Poly a
down_degree (Pol p) = revPol (cut_poly (revPol (Pol p)))

revPol :: Corps a => Poly a -> Poly a
revPol (Pol p) = Pol $ reverse p

cut_poly :: (Eq a, Corps a) => Poly a -> Poly a
cut_poly (Pol []) = Pol []
cut_poly (Pol (p:pr)) | p == neutre = cut_poly (Pol pr)
                      | otherwise = Pol $ (p:pr)
-----------------------------------------------------------------



-----------------------------------------------------------------
--------------------------- Inverse -----------------------------
-----------------------------------------------------------------
-- Inverse de 1 + x mod 1 + x + x³ + x⁴ + x⁸ = x + x² + x⁴ + x⁵ + x⁶ + x⁷
-- Inverse de x² + x³ mod 1 + x + x³ + x⁴ + x⁸ = 

inverse_poly :: (IrreduciblePoly a, Eq a, Corps a) => Poly a -> Poly a
inverse_poly p = calc_restes coefs polys (inverse_poly_aux polyIrr p)
                  where coefs = ((unite, neutre), (neutre, unite))
                        polys = (polyIrr, p)

inverse_poly_aux :: (IrreduciblePoly a, Eq a, Corps a) => Poly a -> Poly a -> [Poly a]
inverse_poly_aux p q | snd p_euclidien == Pol [unite] = [fst p_euclidien]
                     | otherwise = fst p_euclidien : inverse_poly_aux q (snd p_euclidien)
                      where p_euclidien = div_eucli p q

div_eucli :: (IrreduciblePoly a, Eq a, Corps a) => Poly a -> Poly a -> (Poly a, Poly a) -- (quotient, reste)
div_eucli dividende@(Pol d1) diviseur@(Pol d2) | length d1 >= length d2 = (big_quotient, reste)
                                               | otherwise = (Pol [], Pol [])
                                                where big_quotient = operation quotien quotient_recursif
                                                      quotien = create_poly ((length d1 - 1) - (length d2 - 1))
                                                      new_dividende = divise dividende diviseur quotien
                                                      quotient_recursif = fst (div_eucli new_dividende diviseur)
                                                      reste = down_degree $ operation dividende (multi_aux big_quotient diviseur)

calc_restes :: (IrreduciblePoly a, Eq a, Corps a) => ((Poly a, Poly a), (Poly a, Poly a)) -> (Poly a, Poly a) -> [Poly a] -> Poly a
calc_restes (_, (_, cb2)) (_, b) [] = multiplication cb2 b
calc_restes ((ca1, cb1), (ca2, cb2)) (a, b) quotients@(q:qr) = calc_restes ((ca2, cb2), (ca3, cb3)) (a, b) qr
                                                             where ca3 = operation ca1 (multiplication ca2 q)
                                                                   cb3 = operation cb1 (multiplication cb2 q)
-----------------------------------------------------------------

-- pour tester une opé dans la console et pas tout réécrire
toInv = toPoly_Z2Z [0, 0, 1, 1]
pIrr8 = toPoly_Z2Z [1, 1, 0, 1, 1, 0, 0, 0, 1]
q1 = toPoly_Z2Z [1, 0, 1, 1, 1, 1]
q2 = toPoly_Z2Z [0, 1]
q3 = toPoly_Z2Z [1, 1]