{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.Lens.ManySpec (main, spec) where

import Control.Lens
import Data.Diverse
import Data.Diverse.Lens
import Data.Tagged
import Data.Typeable
import Test.Hspec

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

data Foo
data Bar

spec :: Spec
spec = do
    describe "Many" $ do

        it "can converted to and from a tuple using 'Many'' Iso" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
                t = ((5 :: Int), False, 'X', Just 'O')
            x `shouldBe` review _Many' t
            t `shouldBe` view _Many' x

        it "has getter/setter lens using 'item'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            x ^. item @Int `shouldBe` 5
            (x & item @Int .~ 6) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            x ^. item @Bool `shouldBe` False
            (x & item @Bool .~ True) `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ nil
            x ^. item @Char `shouldBe` 'X'
            x ^. item @(Maybe Char) `shouldBe` Just 'O'

        it "has polymorphic getter/setter lens using 'item''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            (x & item' @(Maybe Char) .~ Just 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nil
            (x & item' @Int .~ 'Z') `shouldBe` 'Z' ./ False ./ 'X' ./ Just 'O' ./ nil
            (x & item' @Bool .~ 'Z') `shouldBe` (5 :: Int) ./ 'Z' ./ 'X' ./ Just 'O' ./ nil
            (x & item' @Char .~ True) `shouldBe` (5 :: Int) ./ False ./ True ./ Just 'O' ./ nil
            (x & item' @(Maybe Char) .~ 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ 'P' ./ nil

        it "has getter/setter lens using 'item'" $ do
            let x = (5 :: Int) ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ nil
            x ^. itemL @Foo Proxy `shouldBe` Tagged @Foo False
            (x & itemL @Foo Proxy .~ Tagged @Foo True) `shouldBe` (5 :: Int) ./ Tagged @Foo True ./ Tagged @Bar 'X' ./ nil

        it "has polymorphic getter/setter lens using 'itemL''" $ do
            let x = (5 :: Int) ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ nil
            (x & itemL' @Foo Proxy .~ "foo") `shouldBe` (5 :: Int) ./ "foo" ./ Tagged @Bar 'X' ./ nil

        it "has getter/setter lens for duplicate fields using 'itemN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            x ^. itemN (Proxy @0) `shouldBe` 5
            (x & itemN (Proxy @0) .~ 6) `shouldBe` (6 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            x ^. itemN (Proxy @1) `shouldBe` False
            (x & itemN (Proxy @1) .~ True) `shouldBe` (5 :: Int) ./ True ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            x ^. itemN (Proxy @2) `shouldBe` 'X'
            (x & itemN (Proxy @2) .~ 'O') `shouldBe` (5 :: Int) ./ False ./ 'O' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            x ^. itemN (Proxy @3) `shouldBe` Just 'O'
            (x & itemN (Proxy @3) .~ Just 'P') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'P' ./ (6 :: Int) ./ Just 'A' ./ nil
            x ^. itemN (Proxy @4) `shouldBe` 6
            (x & itemN (Proxy @4) .~ 7) `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (7 :: Int) ./ Just 'A' ./ nil
            x ^. itemN (Proxy @5) `shouldBe` Just 'A'
            (x & itemN (Proxy @5) .~ Just 'B') `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'B' ./ nil

        it "has polymorphic getter/setter lens for duplicate fields using 'itemN''" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            (x & itemN' (Proxy @0) .~ "Foo") `shouldBe` "Foo" ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            (x & itemN' (Proxy @1) .~ "Foo") `shouldBe` (5 :: Int) ./ "Foo" ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            (x & itemN' (Proxy @2) .~ "Foo") `shouldBe` (5 :: Int) ./ False ./ "Foo" ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            (x & itemN' (Proxy @3) .~ "Foo") `shouldBe` (5 :: Int) ./ False ./ 'X' ./ "Foo" ./ (6 :: Int) ./ Just 'A' ./ nil
            (x & itemN' (Proxy @4) .~ "Foo") `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ "Foo" ./ Just 'A' ./ nil
            (x & itemN' (Proxy @5) .~ "Foo") `shouldBe` (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ "Foo" ./ nil

        it "has getter/setter lens for multiple fields using 'project'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            x ^. (project @'[Int, Maybe Char]) `shouldBe` (5 :: Int) ./ Just 'O' ./ nil
            (x & (project @'[Int, Maybe Char]) .~ ((6 :: Int) ./ Just 'P' ./ nil)) `shouldBe`
                (6 :: Int) ./ False ./ 'X' ./ Just 'P' ./ nil

        it "has polymorphic getter/setter lens for multiple fields using 'project'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ nil
            (x & (project' @'[Int, Maybe Char]) .~ ("Foo" ./ Just "Bar" ./ nil)) `shouldBe`
                "Foo" ./ False ./ 'X' ./ Just "Bar" ./ nil

        it "has getter/setter lens for multiple labelled fields using 'projectL'" $ do
            let x = False ./ Tagged @"Hi" (5 :: Int) ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ Tagged @"Bye" 'O' ./ nil
            x ^. (projectL @'[Foo, Bar] Proxy) `shouldBe` Tagged @Foo False ./ Tagged @Bar 'X' ./ nil
            (x & (projectL @'["Hi", "Bye"] Proxy) .~ (Tagged @"Hi" (6 :: Int) ./ Tagged @"Bye" 'P' ./ nil)) `shouldBe`
                False ./ Tagged @"Hi" (6 :: Int) ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ Tagged @"Bye" 'P' ./ nil

        it "has polymorphic getter/setter lens for multiple labelled fields using 'projectL''" $ do
            let x = False ./ Tagged @"Hi" (5 :: Int) ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ Tagged @"Bye" 'O' ./ nil
            (x & (projectL' @'["Hi", "Bye"] Proxy) .~ (True ./ Tagged @"Changed" False ./ nil)) `shouldBe`
                False ./ True ./ Tagged @Foo False ./ Tagged @Bar 'X' ./ Tagged @"Changed" False ./ nil

        it "has getter/setter lens for multiple fields with duplicates using 'projectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            x ^. (projectN @'[5, 4, 0] Proxy) `shouldBe` Just 'A' ./ (6 :: Int) ./ (5 ::Int) ./ nil
            (x & (projectN @'[5, 4, 0] Proxy) .~ (Just 'B' ./ (8 :: Int) ./ (4 ::Int) ./ nil)) `shouldBe`
                (4 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just 'B' ./ nil

        it "has polymorphic getter/setter lens for multiple fields with duplicates using 'projectN'" $ do
            let x = (5 :: Int) ./ False ./ 'X' ./ Just 'O' ./ (6 :: Int) ./ Just 'A' ./ nil
            (x & (projectN' @'[5, 4, 0] Proxy) .~ (Just "Foo" ./ (8 :: Int) ./ "Bar" ./ nil)) `shouldBe`
                "Bar" ./ False ./ 'X' ./ Just 'O' ./ (8 :: Int) ./ Just "Foo" ./ nil
