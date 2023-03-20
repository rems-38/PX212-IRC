module Math_preliminaries where

-- import Group

-- tips Euclide Etendu pour inverse des polynomes

-- newtype Polynome = Poly [Integer]

xor :: Integer -> Integer -> Integer
xor a b | a == b = 0
        | a /= b = 1

addition :: [Integer] -> [Integer] -> [Integer]
addition [] [] = []
addition (n:nr) (m:mr) = (xor n m) : (addition nr mr)