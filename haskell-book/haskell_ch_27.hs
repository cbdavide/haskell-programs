import Debug.Trace (trace)

discriminatory :: Bool -> Int
discriminatory b = 
    case b of
      False -> 0
      True -> 1


data Test = A Test2 | B Test2 deriving (Show)

data Test2 = C Int | D Int deriving (Show)

forceNothing :: Test -> Int
forceNothing _ = 0

forseTest :: Test -> Int
forseTest (A _) = 1
forseTest (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C i)) = i
forceTest2 (B (C i)) = i
forceTest2 (A (D i)) = i
forceTest2 (B (D i)) = i

inc = (+1)
twice = inc . inc

howManyTimes = inc (trace "eval'd" (1 + 1)) + twice (trace "eval'd" (1 + 1))

howManyTimes' =
  let onePlusOne = trace "eval'd" (1 + 1)
   in inc onePlusOne + twice onePlusOne

x = undefined
y = "blah"

main = do
  print (x `seq` snd (x, y))
