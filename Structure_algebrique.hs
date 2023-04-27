module Structure_algebrique where

-----------------------------------------------------------------
--------------------------- Types -------------------------------
-----------------------------------------------------------------
newtype Z_sur_2Z = Z2Z Integer deriving (Show, Eq)
newtype Poly a = Pol [a] deriving (Show, Eq)
-----------------------------------------------------------------



-----------------------------------------------------------------
-------------------------- Classes ------------------------------
-----------------------------------------------------------------
class Groupe a where
      neutre :: a
      unite :: a
      operation :: a -> a -> a
      inverse :: a -> a

class (Groupe a) => Anneau a where
      multiplication :: a -> a -> a

class (Anneau a) => Corps a where
      inverseMultiplicatif :: a -> Maybe a

class (Corps a) => IrreduciblePoly a where
      polyIrr :: Poly a
-----------------------------------------------------------------