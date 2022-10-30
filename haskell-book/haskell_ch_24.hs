{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Bits
import Data.Char
import Data.List
import Data.Ratio ((%))
import Data.Word
import Numeric
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1' <* eof
one' = one >> stop


oneTwo = char '1' >> char '2' <* eof
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main' = do
    pNL "stop:"
    testParse stop

    pNL "one:"
    testParse one

    pNL "one':"
    testParse one'

    pNL "oneTwo:"
    testParse oneTwo

    pNL "oneTwo':"
    testParse oneTwo'

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal

    case denominator of
      0 -> fail "Denominator cannot be zero"
      _ -> return (numerator % denominator)

main :: IO ()
main = do

    let parseFraction' = parseString parseFraction mempty

    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork

    print $ parseFraction' alsoBad
    print $ parseFraction' badFraction


intParser :: Parser Integer
intParser = integer >>= \i -> eof >>= \ eo -> return i 

intParser' :: Parser Integer
intParser' = integer <* eof 

intParser'' :: Parser Integer
intParser'' = do
    num <- integer
    eo <- eof
    return num

intParserTest :: IO ()
intParserTest = do 
    print $ parseString intParser mempty "123"
    print $ parseString intParser' mempty "123"
    print $ parseString intParser'' mempty "123"


type IntegerOrRational = Either Integer Rational

parseIntegerOrRational :: Parser IntegerOrRational
parseIntegerOrRational = (Right <$> try parseFraction) <|> (Left <$> integer)

integerOrRationalTest :: IO ()
integerOrRationalTest = do
    print $ parseString parseIntegerOrRational mempty "1/0"
    print $ parseString parseIntegerOrRational mempty "1/2"
    print $ parseString parseIntegerOrRational mempty "1/2f"
    print $ parseString parseIntegerOrRational mempty "1"
    print $ parseString parseIntegerOrRational mempty "10"
    print $ parseString parseIntegerOrRational mempty "fail"
    print $ parseString parseIntegerOrRational mempty "00000"


parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9']

base10Integer :: Parser Integer
base10Integer = read <$> some parseDigit

base10Integer' :: Parser Integer
base10Integer' = do
    operator <- optional $ char '-'
    num <- base10Integer

    case operator of
      Nothing -> return num
      (Just _) -> return $ negate num

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber = PhoneNumber NumberingPlanArea Exchange LineNumber deriving (Eq, Show)

dashSeparated :: Parser PhoneNumber
dashSeparated  = do
    planArea <- read <$> count 3 digit
    char '-'
    exchange <- read <$> count 3 digit
    char '-'
    number <- read <$> count 4 digit
    return $ PhoneNumber planArea exchange number

normalForm :: Parser PhoneNumber
normalForm = do
    planArea <- read <$> count 3 digit
    exchange <- read <$> count 3 digit
    number <- read <$> count 4 digit
    return $ PhoneNumber planArea exchange number

specialForm :: Parser PhoneNumber
specialForm = do
    planArea <- read <$> between (char '(') (char ')') (count 3 digit)
    space
    exchange <- read <$> count 3 digit
    char '-'
    number <- read <$> count 4 digit
    return $ PhoneNumber planArea exchange number

parsePhone :: Parser PhoneNumber
parsePhone = try normalForm <|> dashSeparated <|> specialForm

checkPhone :: IO ()
checkPhone = do
    let parser = parseString parsePhone mempty
    print $ parser "123-456-7890"
    print $ parser "1234567890"
    print $ parser "(123) 456-7890"


data IPAddress = IPAddress Word32 deriving (Eq, Ord)
data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)

-- this function extracts `e` bits starting from a given position `b`
-- source: https://geeksforgeeks.org/extract-k-bits-given-position-number/
extractWord :: (Bits a, Num a ) => a -> Int -> Int -> a
extractWord n b e = ((1 `shiftL` e) - 1) .&. (n `shiftR` (b - 1))

extractOctet :: Word32 -> Int -> Word32
extractOctet n b = extractWord n b 8

extractHex :: Word64 -> Int -> Word64
extractHex n b = extractWord n b 16

word64ToHex :: Word64 -> String
word64ToHex n = showIntAtBase 16 intToDigit n ""

instance Show IPAddress where
    show (IPAddress num) = intercalate "." $ fmap show octets
        where octets = fmap (extractOctet num) [25, 17, 9, 1] 

instance Show IPAddress6 where
    show (IPAddress6 most less) = intercalate ":" $ fmap word64ToHex (mconcat [m, l])
        where m = fmap (extractHex most) [49, 33, 17, 1] 
              l = fmap (extractHex less) [49, 33, 17, 1] 

parseOctet :: Parser Word8
parseOctet = do
    num <- integer

    if num > 255 
       then fail $ "Literal " ++ show num ++ " is out of the octet range 0..255"
       else return (read . show $ num)


buildIPAddress :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
buildIPAddress a b c d = (a `shiftL` 24) .|. (b `shiftL` 16) .|. (c `shiftL` 8) .|. d

parseIPAddress :: Parser IPAddress
parseIPAddress = do
    a <- fromIntegral <$> parseOctet
    char '.'
    b <- fromIntegral <$> parseOctet
    char '.'
    c <- fromIntegral <$> parseOctet
    char '.'
    d <- fromIntegral <$> parseOctet
    
    return $ IPAddress $ buildIPAddress a b c d

hexToWord16 :: String -> Word16
hexToWord16 s = fst $ readHex s !! 0

parseGroup :: Parser Word16
parseGroup = do
    group <- some hexDigit
    
    if length group > 4 
       then fail "A group can contain between 1 and 4 hex digits"
       else return $ hexToWord16 group

dotSeparatedGroups :: Parser [Word16]
dotSeparatedGroups = sepBy parseGroup (char ':')

dotSeparatedGroups' :: Parser [Word16]
dotSeparatedGroups' = endBy parseGroup (char ':')

parseUncompressedGroups :: Parser [Word16]
parseUncompressedGroups = do
    groups <- dotSeparatedGroups
    if length groups /= 8
       then fail "A full IPv6 address should have 8 groups"
       else return groups

buildMissingGroups :: Int -> [Word16]
buildMissingGroups n = take n $ repeat 0

parseCompressedGroups :: Parser [Word16]
parseCompressedGroups = do
    groups1 <- dotSeparatedGroups'

    case null groups1 of
      True -> string "::"
      False -> string ":"

    groups2 <- dotSeparatedGroups

    let totalGroups = length groups1 + length groups2

    if totalGroups > 7
       then fail "There is no space for compressed groups"
       else return $ groups1 ++ buildMissingGroups (8 - totalGroups) ++ groups2


buildWord64 :: [Word64] -> Word64
buildWord64 xs = foldr (\x  y -> x .|. y) 0 ds
    where ds = fmap (\(x, y) -> x `shiftL` y) $ zip xs (fmap (*16) (reverse [0..3]))

buildIPAddress6 :: [Word16] -> IPAddress6
buildIPAddress6 ds = IPAddress6 mostSignificant lessSignificant
    where mostSignificant = buildWord64 $ take 4 (fmap fromIntegral ds)
          lessSignificant = buildWord64 $ take 4 (fmap fromIntegral $ drop 4 ds)

parseIPAddress6 :: Parser IPAddress6
parseIPAddress6 = do
    groups <- try parseUncompressedGroups <|> parseCompressedGroups
    return $ buildIPAddress6 groups
