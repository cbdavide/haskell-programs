eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd a b
    | a > b  = []
    | a == b = [a]
    | otherwise = a : eftOrd (succ a) b
