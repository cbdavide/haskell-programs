import Criterion.Main

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import qualified System.IO as SIO

infixl 9 !?
(!?) :: [a] -> Int -> Maybe a
_       !? n | n < 0    = Nothing
[]      !? _            = Nothing
(x:_)   !? 0            = Just x
(_:xs)  !? n            = xs !? (n - 1)

myList :: [Int]
myList = [1..9999]

main' :: IO ()
main' = defaultMain
    [ bench "index list 9999" $ whnf (myList !!) 9998
    , bench "index list maybe index 9999" $ whnf (myList !?) 9998
    ]

f :: IO ()
f = do
    print ([1..] !! 999999)
    putStrLn "f"

g :: IO ()
g = do
    print ([1..] !! 9999999)
    putStrLn "g"


main'' :: IO ()
main'' = do
    f
    g


dictWords :: IO String
dictWords = SIO.readFile "/usr/share/dict/words"

dictWordsT :: IO T.Text
dictWordsT = TIO.readFile "/usr/share/dict/words"

dictWordsTL :: IO TL.Text
dictWordsTL = TLIO.readFile "/usr/share/dict/words"

main :: IO ()
main = do
    replicateM_ 1000 (dictWords >>= print)
    replicateM_ 1000 (dictWordsT >>= TIO.putStrLn)
    replicateM_ 1000 (dictWordsTL >>= TLIO.putStrLn)
