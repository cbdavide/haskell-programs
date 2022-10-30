{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Map as M
import Data.Time (Day, fromGregorian, TimeOfDay (..))
import Journal
import Test.Hspec
import Text.Trifecta
import Text.RawString.QQ

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

twoBlockSegment :: ByteString
twoBlockSegment = [r|# 2022-04-16
08:00 Breakfast
10:00 Lunch
|]

twoBlockSegmentWithComments :: ByteString
twoBlockSegmentWithComments = [r|
-- just a comment
# 2022-04-16 -- inline comments

08:30 Breakfast -- more inline comments
10:00 Lunch
|]


simpleJournal :: ByteString
simpleJournal = [r|
-- just a comment
# 2022-04-16
08:30 Breakfast -- hello
10:00 Lunch

# 2022-04-17
06:00 Wake up!!
10:00 Go to bed :)


-- Its my birthday
# 2022-04-21
00:00 Do nothing -- yes, my favorite
|]

sumJournal :: ByteString
sumJournal = [r|
# 2022-04-17
00:00 First
01:00 Second
02:00 Third

# 2022-04-18
00:00 First
02:00 Second
04:26 Third
|]

main :: IO ()
main = hspec $ do
    
    describe "parseSegment" $ do
        let parser = parseByteString parseSegment mempty

        it "can parse segment with more than one log" $ do
            let r = maybeSuccess $ parser twoBlockSegment
                expected = (
                    fromGregorian 2022 4 16, 
                    [Log (TimeOfDay 8 0 0) "Breakfast", Log (TimeOfDay 10 0 0) "Lunch"])
            r `shouldBe` (Just expected)

        it "can parse segment and ignores comments and empty lines" $ do
            let r = maybeSuccess $ parser twoBlockSegmentWithComments 
                expected = (
                    fromGregorian 2022 04 16, 
                    [Log (TimeOfDay 8 30 0) "Breakfast", Log (TimeOfDay 10 0 0) "Lunch"])
            r `shouldBe` (Just expected)

    describe "parseJournal" $ do
        let parser = parseByteString parseJournal mempty

        it "can parse journal with many days and comments" $ do
            let r = maybeSuccess $ parser simpleJournal
                expected = M.fromList 
                    [
                      (fromGregorian 2022 04 16, [Log (TimeOfDay 8 30 0) "Breakfast", Log (TimeOfDay 10 0 0) "Lunch"]) 
                    , (fromGregorian 2022 04 17, [Log (TimeOfDay 6 0 0) "Wake up!!", Log (TimeOfDay 10 0 0) "Go to bed :)"]) 
                    , (fromGregorian 2022 04 21, [Log (TimeOfDay 0 0 0) "Do nothing"])]

            r `shouldBe` (Just expected)

    describe "calcActivitiesTimeSum" $ do
        let journal = maybeSuccess $ parseByteString parseJournal mempty sumJournal

        it "returns activities length sum" $ do

            let func (a, b) = calcActivitiesTimeSum a b
            let values = map func (M.toList $ fromJust journal)

            values `shouldBe` [7200, 15960]
