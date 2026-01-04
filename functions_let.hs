-- functions let bindings
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + (2 * topArea)

cylinder' :: (RealFloat a) => a -> a -> a
cylinder' r h = sideArea + (2 * topArea)
  where
    sideArea = 2 * pi * r * h
    topArea = pi * r ^ 2

calcDensities :: (RealFloat a) => [(a, a)] -> [a]
calcDensities xs =
  [density | (m, v) <- xs, let density = m / v, density > air && density < water]
  where
    (air, water) = (1.2, 1000)

-- let boot x y z = x * y * z in boot 2 3 4