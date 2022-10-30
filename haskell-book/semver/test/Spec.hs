{-# LANGUAGE OverloadedStrings #-}

import SemVer
import Test.Hspec
import Text.Trifecta

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do

  describe "numericIdentifier" $ do

    let parser = parseByteString (numericIdentifier <* eof) mempty

    it "fails when identifier has leading zeros" $ do
      let r = maybeSuccess $ parser "001"
      r `shouldBe` Nothing

    it "fails when identifier has non digits characters" $ do
      let r = maybeSuccess $ parser "0a01"
      r `shouldBe` Nothing

    it "succeeds when identifier is zero" $ do
      let r = maybeSuccess $ parser "0"
      r `shouldBe` (Just "0")

    it "succeeds when identifier is a valid number" $ do
      let r = maybeSuccess $ parser "12345"
      r `shouldBe` (Just "12345")

  describe "versionCore" $ do

    let parser = parseByteString (versionCore <* eof) mempty

    it "fails when major is not a valid numeric identifier" $ do
      let r = maybeSuccess $ parser "00.1.2"
      r `shouldBe`Nothing

    it "fails when minor is not a valid numeric identifier" $ do
      let r = maybeSuccess $ parser "10.aa.2"
      r `shouldBe`Nothing

    it "fails when patch is not a valid numeric identifier" $ do
      let r = maybeSuccess $ parser "10.1.bad"
      r `shouldBe`Nothing

    it "fails when minor and patch are missing" $ do
      let r = maybeSuccess $ parser "10"
      r `shouldBe`Nothing

    it "fails when is missing" $ do
      let r = maybeSuccess $ parser "10"
      r `shouldBe`Nothing

    it "succeeds when components are valid numeric identifiers" $ do
      let r = maybeSuccess $ parser "10.10.10"
      r `shouldBe` (Just (10, 10, 10))

  describe "alphaNumericIdentifier" $ do

    let parser = parseByteString (alphaNumericIdentifier <* eof) mempty

    it "fails when identifier is a number" $ do
      let r = maybeSuccess $ parser "1"
      r `shouldBe` Nothing

    it "fails when identifier contains ?" $ do
      let r = maybeSuccess $ parser "a?"
      r `shouldBe` Nothing

    it "fails when identifier contains ." $ do
      let r = maybeSuccess $ parser "b."
      r `shouldBe` Nothing

    it "succeeds when identifier starts with digits but it ends on letters" $ do
      let r = maybeSuccess $ parser "1a"
      r `shouldBe` (Just "1a")

    it "succeeds when identifer ends with digits" $ do
      let r = maybeSuccess $ parser "a11"
      r `shouldBe` (Just "a11")

    it "succeeds when identifer contains a number with leading zeros" $ do
      let r = maybeSuccess $ parser "a011"
      r `shouldBe` (Just "a011")

  describe "releaseIdentifier" $ do
    let parser = parseByteString (releaseIdentifier <* eof) mempty

    it "succeeds when identifer is a valid alpa numeric identifier" $ do
      let r = maybeSuccess $ parser "a011"
      r `shouldBe` (Just $ NOSS "a011")

    it "succeeds when identifer is a valid numeric identifier" $ do
      let r = maybeSuccess $ parser "11"
      r `shouldBe` (Just $ NOSI 11)

    it "fails when identifer is not a valid numeric identifier" $ do
      let r = maybeSuccess $ parser "011"
      r `shouldBe` Nothing

  describe "metadataIdentifier" $ do
    let parser = parseByteString (metadataIdentifier <* eof) mempty

    it "succeeds when identifer is a valid alpa numeric identifier" $ do
      let r = maybeSuccess $ parser "a011"
      r `shouldBe` (Just $ NOSS "a011")

    it "succeeds when identifer has leading zeros" $ do
      let r = maybeSuccess $ parser "011"
      r `shouldBe` (Just $ NOSS "011")

  describe "releaseSegment" $ do
    let parser = parseByteString (releaseSegment <* eof) mempty

    it "succeeds with valid dot separated release identifiers" $ do
      let r = maybeSuccess $ parser "pre-release.1.alpha"
      r `shouldBe` (Just [NOSS "pre-release", NOSI 1, NOSS "alpha"])

    it "fails when valid release identifiers are not separated by a dot" $ do
      let r = maybeSuccess $ parser "pre-release 1 alpha"
      r `shouldBe` Nothing

    it "fails when no segments are given" $ do
      let r = maybeSuccess $ parser ""
      r `shouldBe` Nothing

  describe "metadataSegment" $ do
    let parser = parseByteString (metadataSegment <* eof) mempty

    it "succeeds with valid dot separated release identifiers" $ do
      let r = maybeSuccess $ parser "pre-release.001.alpha"
      r `shouldBe` (Just [NOSS "pre-release", NOSS "001", NOSS "alpha"])

    it "fails when valid release identifiers are not separated by a dot" $ do
      let r = maybeSuccess $ parser "pre-release 001 alpha"
      r `shouldBe` Nothing

    it "fails when no segments are given" $ do
      let r = maybeSuccess $ parser ""
      r `shouldBe` Nothing

  describe "semVer" $ do 

    let parser = parseByteString (semVer <* eof) mempty

    it "succeeds when no release and metadata segments are given" $ do
      let r = maybeSuccess $ parser "1.0.0"
      r `shouldBe` (Just $ SemVer 1 0 0 [] [])

    it "succeeds when no release is given and metadata is given" $ do
      let r = maybeSuccess $ parser "1.2.3+build-001"
      r `shouldBe` (Just $ SemVer 1 2 3 [] [NOSS "build-001"])

    it "succeeds when no metadata is given and release is given" $ do
      let r = maybeSuccess $ parser "100.7.20-build.1"
      r `shouldBe` (Just $ SemVer 100 7 20 [NOSS "build", NOSI 1] [])

    it "succeeds when release and metadata are given" $ do
      let r = maybeSuccess $ parser "100.7.20-build.1+alpha"
      r `shouldBe` (Just $ SemVer 100 7 20 [NOSS "build", NOSI 1] [NOSS "alpha"])

