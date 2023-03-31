-- Test de la fonction addMod2 
-- Test de la fonction oppose2
putStrLn "=== Test addMod2 ==="
addMod2 (Z2Z 1) (Z2Z 1) -- Z2Z 0
addMod2 (Z2Z 1) (Z2Z 0) -- Z2Z 1
addMod2 (Z2Z 0) (Z2Z 0) -- Z2Z 0
putStrLn "=== Test oppose2 ==="
oppose2 (Z2Z 1) -- Z2Z 1
oppose2 (Z2Z 0) -- Z2Z 0

-- Test de la fonction toGF
testToGF = do
  putStrLn "=== Test toGF ==="
  print $ toGF [1, 0, 1, 1] -- Gf [Z2Z 1, Z2Z 0, Z2Z 1, Z2Z 1]
  print $ toGF [0, 1, 0] -- Gf [Z2Z 0, Z2Z 1, Z2Z 0]

-- Test de l'instance Group de Z_sur_2Z
testZ2ZGroup = do
  let a = Z2Z 1
      b = Z2Z 0
      c = Z2Z 1
  putStrLn "=== Test Group instance for Z_sur_2Z ==="
  putStrLn "Associativity: "
  print $ (a `operation` b) `operation` c == a `operation` (b `operation` c) -- True
  putStrLn "Identity: "
  print $ a `operation` unit == a && unit `operation` a == a -- True
  putStrLn "Inverse: "
  print $ a `operation` inverse a == unit && inverse a `operation` a == unit -- True

-- Test de l'instance Group de GF
testGFGroup = do
  let a = toGF [1, 0, 1, 1]
      b = toGF [0, 1, 0]
      c = toGF [1, 1, 1]
  putStrLn "=== Test Group instance for GF ==="
  putStrLn "Associativity: "
  print $ (a `operation` b) `operation` c == a `operation` (b `operation` c) -- True
  putStrLn "Identity: "
  print $ a `operation` unit == a && unit `operation` a == a -- True
  putStrLn "Inverse: "
  print $ a `operation` inverse a == unit && inverse a `operation` a == unit -- True

-- Tests unitaires
main = do 
  testZ2Z
  testToGF
  testZ2ZGroup
  testGFGroup