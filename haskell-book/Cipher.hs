module Cipher where

import Data.Char
import Data.List

cipherBase :: Char -> Char -> Int -> Char
cipherBase b l n = chr $ mod ((ord l - ord b) + n) 26 + ord b

cipherLower :: Char -> Int -> Char
cipherLower = cipherBase 'a'

cipherUpper :: Char -> Int -> Char
cipherUpper = cipherBase 'A'

cipher :: String -> Int -> String
cipher "" _     = ""
cipher ts 0     = ts
cipher (t:ts) n = inner t n ts
    where inner t n ts 
            | isUpper t = cipherUpper t n : cipher ts n
            | isLower t = cipherLower t n : cipher ts n
            | otherwise = t : cipher ts n


decipher :: String -> Int -> String
decipher text n = cipher text (negate n)

cipher' :: Char -> Char -> Char
cipher' a k = cipherLower a $ ord k - ord 'a'

vigenereCipher :: String -> String -> String
vigenereCipher plainText key = unwords $ inner (words plainText) (cycle key)
    where inner :: [String] -> String -> [String]
          inner [] _ = []
          inner (x:xs) y = zipWith cipher' x y : inner xs (drop (length x) y)
