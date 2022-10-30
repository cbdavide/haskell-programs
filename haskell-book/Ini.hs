{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Test.Hspec

import Text.RawString.QQ
import Text.Trifecta

type Name = String
type Value = String
type Assignments = Map Name Value

newtype Header = Header String deriving (Eq, Ord, Show)
newtype Config = Config (Map Header Assignments) deriving (Eq, Show)

data Section = Section Header Assignments deriving (Eq, Show)


parseBracketPair :: Parser a -> Parser a
parseBracketPair p =
    char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment = do
    name <- some letter
    _ <- char '='
    val <- some (noneOf "\n")
    skipEOL
    return (name, val)

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipComments :: Parser ()
skipComments = skipMany $ do 
    _ <- char ';' <|> char '#'
    skipMany (noneOf "\n")
    skipEOL

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
    skipWhitespace
    skipComments

    h <- parseHeader
    skipEOL

    assignments <- some parseAssignment
    return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m = M.insert h a m

parseIni :: Parser Config
parseIni = do
    sections <- some parseSection
    let mapOfSections = foldr rollup M.empty sections
    return $ Config mapOfSections


sectionEx :: ByteString
sectionEx = "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
;ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandlcaw
|]

-- main :: IO ()
-- main = do
--     print $ parseString parseSection mempty sectionEx
