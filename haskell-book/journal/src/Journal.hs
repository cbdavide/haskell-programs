{-# LANGUAGE OverloadedStrings #-}

module Journal where

import Control.Applicative
import Data.Time (
    Day,
    fromGregorian,
    NominalDiffTime,
    diffLocalTime,
    TimeOfDay (..),
    LocalTime (..))

import Data.Map (Map)
import qualified Data.Map as M
import Text.Trifecta

type Description = String
type Journal = Map Day [Log]

data Log = Log TimeOfDay Description deriving (Eq, Show)


skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

skipComment :: Parser ()
skipComment  = do
    skipMany (oneOf " ")
    string "-- "
    skipMany (noneOf "\n")

skipMultipleComments :: Parser ()
skipMultipleComments = skipMany skipComment

parseMonth :: Parser Int
parseMonth = do
    month <- count 2 digit

    if (read month) > 12 
       then fail "Invalid month"
       else return (read month)

parseDay :: Parser Int
parseDay = do
    day <- count 2 digit

    if (read day) > 31 
       then fail "Invalid day"
       else return (read day)

parseYear :: Parser Integer
parseYear = do
    year <- count 4 digit
    return (read year)

parseDate :: Parser Day
parseDate = do
    year <- parseYear
    char '-'
    month <- parseMonth
    char '-'
    day <- parseDay
    
    return $ fromGregorian year month day

parseHour :: Parser Int
parseHour = do
    hour <- count 2 digit

    if (read hour) > 23
       then fail "Invalid hour"
       else return (read hour)

parseMinute :: Parser Int
parseMinute = do
    minute <- count 2 digit

    if (read minute) > 59
       then fail "Invalid minute"
       else return (read minute)

parseTime :: Parser TimeOfDay
parseTime = do
    hour <- parseHour
    char ':'
    minute <- parseMinute
    return (TimeOfDay hour minute 0)


parseHeader :: Parser Day
parseHeader = do
    char '#'
    whiteSpace

    date <- parseDate

    optional skipComment
    skipEOL

    return date

parseDescription :: Parser String
parseDescription = do
    try (manyTill (noneOf "\n") (try skipComment)) <|> many (noneOf "\n")

parseLog :: Parser Log
parseLog = do

    time <- parseTime
    whiteSpace

    description <- parseDescription

    skipEOL

    return $ Log time description

parseSegment :: Parser (Day, [Log])
parseSegment = do
    skipWhitespace 
    skipMultipleComments 
    skipWhitespace 

    date <- parseHeader
    logs <- some parseLog

    return (date, logs)

rollup :: (Day, [Log]) -> Journal -> Journal
rollup (date, logs) journal = M.insert date logs journal

parseJournal :: Parser Journal
parseJournal = do
    segments <- some parseSegment

    let mapOfSegments = foldr rollup M.empty segments
    return mapOfSegments


calcActivitiesTimeSum :: Day -> [Log] -> NominalDiffTime
calcActivitiesTimeSum d logs = foldr (+) 0 $ calNominalDiffTime (transformLogTimes d logs)

calcActivitiesTimeAvg :: Day -> [Log] -> NominalDiffTime
calcActivitiesTimeAvg d logs = (calcActivitiesTimeSum d logs) / fromIntegral (length logs)

transformLogTimes :: Day -> [Log] -> [LocalTime]
transformLogTimes d logs = LocalTime d <$> (map (\(Log t _) -> t) logs)

calNominalDiffTime :: [LocalTime] -> [NominalDiffTime]
calNominalDiffTime dates = zipWith diffLocalTime (tail dates) dates
