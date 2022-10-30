{-# LANGUAGE OverloadedStrings #-}

module SemVer where

import Control.Applicative
import Text.Trifecta


data NumberOrString = NOSS String | NOSI Integer deriving (Show, Eq, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer { major :: Major
                     , minor :: Minor 
                     , patch :: Patch 
                     , releaseInfo :: Release
                     , metadataInfo :: Metadata
                     } deriving (Show, Eq)


instance Ord SemVer where
    compare a b
      | major a > major b = GT
      | major a < major b = LT
      | minor a > minor b = GT
      | minor a < minor b = LT
      | patch a > patch b = GT
      | patch a < patch b = LT
      | null (releaseInfo a) && not (null $ releaseInfo b) = GT
      | not (null $ releaseInfo a) && null (releaseInfo b) = LT
      | releaseInfo a > releaseInfo b = GT
      | releaseInfo a < releaseInfo b = LT
      | otherwise = EQ


positiveDigit :: Parser Char
positiveDigit  = oneOf ['1'..'9']

positiveDigitAndDigits :: Parser String
positiveDigitAndDigits = do
    d <- positiveDigit
    ds <- many digit
    return $ d : ds

numericIdentifier :: Parser String
numericIdentifier =
    try $ string "0" <|> positiveDigitAndDigits

majorSegment :: Parser Major
majorSegment = read  <$> numericIdentifier

minorSegment :: Parser Minor
minorSegment = read <$> numericIdentifier

patchSegment :: Parser Patch
patchSegment = read <$> numericIdentifier

versionCore :: Parser (Major, Minor, Patch)
versionCore = do
    mj <- majorSegment
    _ <- char '.'
    mn <- minorSegment
    _ <- char '.'
    ptch <- patchSegment
    return $ (mj, mn, ptch)

nonDigit :: Parser Char
nonDigit = letter <|> char '-'

alphaNumericIdentifier :: Parser String
alphaNumericIdentifier =
    (some digit >>= \as -> some nonDigit >>= \bs -> many digit >>= \cs -> return $ as ++ bs ++ cs)
    <|> (some nonDigit >>= \as -> many digit >>= \bs -> return $ as ++ bs)

releaseIdentifier :: Parser NumberOrString 
releaseIdentifier =
    try (NOSS <$> alphaNumericIdentifier) <|> (NOSI . read <$> numericIdentifier)

metadataIdentifier :: Parser NumberOrString
metadataIdentifier =
    try (NOSS <$> alphaNumericIdentifier) <|> (NOSS <$> some digit)

releaseSegment :: Parser Release
releaseSegment = sepBy1 releaseIdentifier (char '.')

metadataSegment :: Parser Metadata
metadataSegment = sepBy1 metadataIdentifier (char '.')

semVer :: Parser SemVer
semVer = do
    (mj, mn, ptch) <- versionCore

    releaseSegment' <- optional $ char '-' >> releaseSegment
    metadataSegment' <- optional $ char '+' >> metadataSegment

    let releaseSegment'' = 
            case releaseSegment' of
              Nothing -> []
              (Just segments) -> segments

    let metadataSegment'' = 
            case metadataSegment' of
              Nothing -> []
              (Just metadata) -> metadata


    return $ SemVer { major = mj
                    , minor = mn
                    , patch = ptch
                    , releaseInfo = releaseSegment''
                    , metadataInfo = metadataSegment''
                    }
