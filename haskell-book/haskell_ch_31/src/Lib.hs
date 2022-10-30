{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Text (Text)
import qualified Data.Text as T

import Types

unmarshalUser :: Text -> Maybe User
unmarshalUser t = process $ T.splitOn ":" t
  where
    process :: [Text] -> Maybe User
    process xs
      | length xs /= 5 = Nothing 
      | otherwise = Just $ User 0 (xs !! 0) (xs !! 1) (xs !! 2) (xs !! 3) (xs !! 4)
