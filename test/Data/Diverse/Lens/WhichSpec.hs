{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Diverse.Lens.WhichSpec (main, spec) where

import Control.Lens
import Data.Diverse
import Data.Diverse.Lens
import Data.Tagged
import Test.Hspec

data Foo
data Bar
data Hi
data Bye

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- -- | Utility to convert Either to Maybe
-- hush :: Either a b -> Maybe b
-- hush = either (const Nothing) Just

spec :: Spec
spec = do
    describe "Which" $ do

        it "can be constructed and destructed by type with 'facet'" $ do
            let y = review (facet @Int) (5 :: Int) :: Which '[Bool, Int, Char, Bool, Char]
                x = preview (facet @Int) y
            x `shouldBe` (Just 5)

        it "can be constructed and destructed by label with 'facetL'" $ do
            let y = review (facetL @Bar) (Tagged (5 :: Int)) :: Which '[Tagged Foo Bool, Tagged Bar Int, Char, Bool, Char]
                x = preview (facetL @Bar) y
                z = preview (facetL @Foo) y
            x `shouldBe` Just (Tagged 5)
            z `shouldBe` Nothing

        it "can be constructed and destructed by label with 'facetTag'" $ do
            let y = review (facetTag @Bar) (5 :: Int) :: Which '[Tagged Foo Bool, Tagged Bar Int, Char, Bool, Char]
                x = preview (facetTag @Bar) y
                z = preview (facetTag @Foo) y
            x `shouldBe` Just 5
            z `shouldBe` Nothing

        it "can be constructed and destructed by index with 'facetN'" $ do
            let y = review (facetN @4) (5 :: Int) :: Which '[Bool, Int, Char, Bool, Int, Char]
                x = preview (facetN @4) y
            x `shouldBe` (Just 5)

        it "can be 'diversify'ed and 'reinterpreted' by type with 'inject'" $ do
            let x = pick (5 :: Int) :: Which '[String, Int]
                y = review (inject @_ @[Bool, Int, Char, String]) x
            y `shouldBe` pick (5 :: Int)
            let y' = preview (inject @[String, Int]) y
            y' `shouldBe` Just (pick (5 :: Int))

        it "can be 'diversifyL'ed and 'reinterpretedL' by label with 'injectL'" $ do
            let t = pick @_ @[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool] (5 :: Tagged Bar Int)
                b = pick @_ @'[Tagged Foo Bool, Tagged Bar Int] (5 :: Tagged Bar Int)
                t' = review (injectL @[Foo, Bar] @_ @[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool]) b
                b' = preview (injectL @[Foo, Bar]) t'
            t `shouldBe` t'
            b' `shouldBe` Just b

        it "can be 'diversifyN'ed and 'reinterpretedN' by index with 'injectN'" $ do
            let x = pick (5 :: Int) :: Which '[String, Int]
                y = review (injectN @[3, 1] @_ @[Bool, Int, Char, String]) x
            y `shouldBe` pick (5 :: Int)
            let y' = preview (injectN @[3, 1] @[String, Int]) y
            y' `shouldBe` Just (pick (5 :: Int))
