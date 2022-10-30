import Criterion.Main
import Data.Maybe
import Data.Sequence ((|>), (<|))

import qualified Data.Sequence as S

data Queue a = Queue { enqueue :: [a], dequeue :: [a] } 
  deriving (Eq, Show)

empty :: Queue a
empty = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue enq deq) = Queue (x:enq) deq 

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop (Queue deq []) = pop $ Queue [] (reverse deq)
pop (Queue deq (x:xs)) = Just (x, Queue deq xs)  


alternatePushNPop2to1 :: Int -> Queue Int
alternatePushNPop2to1 n = go n empty
  where go 0 xs = xs
        go n xs
          | even (n `mod` 3) = go (n - 1) (push n xs)
          | otherwise = go (n - 1) (snd $ fromMaybe (-1, empty) (pop xs))


alternatePushNPop1to1 :: Int -> Queue Int
alternatePushNPop1to1 n = go n empty
  where go 0 xs = xs
        go n xs
          | even n = go (n - 1) (push n xs)
          | otherwise = go (n - 1) (snd $ fromMaybe (-1, empty) (pop xs))


listAlternatePushNPop1to1 :: Int -> [Int]
listAlternatePushNPop1to1 n = go n []
  where go 0 xs = xs
        go n xs
          | even n = go (n - 1) (xs ++ [n])
          | otherwise = go (n - 1) (if length xs > 0 then tail xs else [])


sequenceAlternatePushNPop1to1 :: Int -> S.Seq Int
sequenceAlternatePushNPop1to1 n = go n S.empty
  where go 0 xs = xs
        go n xs
          | even n = go (n - 1) (xs |> n)
          | otherwise = go (n - 1) (S.drop 1 xs)

main :: IO ()
main = defaultMain
    [ bench "push n' pop queue" $ whnf alternatePushNPop1to1 123456
    , bench "push n' pop queue 2to1" $ whnf alternatePushNPop2to1 123456
    , bench "push n' pop list" $ whnf listAlternatePushNPop1to1 123456
    , bench "push n' pop sequence" $ whnf sequenceAlternatePushNPop1to1 123456
    ]
