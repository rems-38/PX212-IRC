module Structure_algebrique where


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
      inverseMultiplicatif :: a -> a
-----------------------------------------------------------------