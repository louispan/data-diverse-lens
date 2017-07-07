{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.Diverse.Lens.Which (
      -- * Single type
      -- ** Prism
      facet
    , facetL
    , facetN

      -- * Multiple types
      -- ** Prism
    , inject
    , injectL
    , injectN

    ) where

import Control.Lens
import Data.Diverse.Which
import Data.Diverse.TypeLevel

-----------------------------------------------------------------

-- | Utility to convert Either to Maybe
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-----------------------------------------------------------------

-- | 'pick' ('review' 'facet') and 'trial' ('preview' 'facet') in 'Prism'' form.
--
-- @
-- 'facet' = 'prism'' 'pick' (either (const Nothing) Just . 'trial')
-- @
--
-- @
-- let y = 'review' ('facet' \@Int) (5 :: Int) :: 'Which' '[Bool, Int, Char, Bool, Char] -- 'pick'
--     x = 'preview' ('facet' \@Int) y -- 'trial'
-- x \`shouldBe` (Just 5)
-- @
facet :: forall x xs. (UniqueMember x xs) => Prism' (Which xs) x
facet = prism' pick (hush . trial)

-- | 'pickL' ('review' 'facetL') and 'trialL' ('preview' 'facetL') in 'Prism'' form.
--
-- @
-- let y = 'review' ('facetL' \@Bar Proxy) (Tagged (5 :: Int)) :: Which '[Tagged Foo Bool, Tagged Bar Int, Char, Bool, Char]
--     x = 'preview' ('facetL' \@Bar Proxy) y
-- x \`shouldBe` (Just (Tagged 5))
-- @
facetL :: forall l xs x proxy. (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => proxy l -> Prism' (Which xs) x
facetL p = prism' (pickL p) (hush . trialL p)

-- | 'pickN' ('review' 'facetN') and 'trialN' ('preview' 'facetN') in 'Prism'' form.
--
-- @
-- 'facetN' p = 'prism'' ('pickN' p) (either (const Nothing) Just . 'trialN' p)
-- @
--
-- @
-- let y = 'review' ('facetN' (Proxy \@4)) (5 :: Int) :: 'Which' '[Bool, Int, Char, Bool, Int, Char] -- 'pickN'
--     x = 'preview' ('facetN' (Proxy \@4)) y -- 'trialN'
-- x \`shouldBe` (Just 5)
-- @
facetN :: forall n xs x proxy. (MemberAt n x xs) => proxy n -> Prism' (Which xs) x
facetN p = prism' (pickN p) (hush . trialN p)

------------------------------------------------------------------


-- | 'diversify' ('review' 'inject') and 'reinterpret' ('preview' 'inject') in 'Prism'' form.
--
-- @
-- let x = 'pick' (5 :: Int) :: 'Which' '[String, Int]
--     y = 'review' ('inject' \@_ \@[Bool, Int, Char, String]) x -- 'diversify'
-- y \`shouldBe` pick (5 :: Int) :: 'Which' '[Bool, Int, Char, String]
-- let y' = 'preview' ('inject' \@[String, Int]) y -- 'reinterpret'
-- y' \`shouldBe` Just (pick (5 :: Int)) :: Maybe ('Which' '[String, Int])
-- @
inject
    :: forall branch tree.
       ( Diversify tree branch
       , Reinterpret branch tree
       )
    => Prism' (Which tree) (Which branch)
inject = prism' diversify (hush . reinterpret)

-- | 'diversifyL' ('review' 'injectL') and 'reinterpretL' ('preview' 'injectL') in 'Prism'' form.
--
-- @
-- let t = 'pick' \@[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool] (5 :: Tagged Bar Int)
--     b = 'pick' \@'[Tagged Foo Bool, Tagged Bar Int] (5 :: Tagged Bar Int)
--     t' = 'review' ('injectL' \@[Foo, Bar] \@_ \@[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool] Proxy) b
--     b' = 'preview' ('injectL' \@[Foo, Bar] Proxy) t'
-- t \`shouldBe` t'
-- b' \`shouldBe` Just b
-- @
injectL
    :: forall ls branch tree proxy.
       ( Diversify tree branch
       , Reinterpret branch tree
       , branch ~ KindsAtLabels ls tree
       , UniqueLabels ls tree
       , IsDistinct ls
       )
    => proxy ls -> Prism' (Which tree) (Which branch)
injectL p = prism' (diversifyL p) (hush . reinterpretL p)

-- | 'diversifyN' ('review' 'injectN') and 'reinterpretN' ('preview' 'injectN') in 'Prism'' form.
--
-- @
-- let x = 'pick' (5 :: Int) :: 'Which' '[String, Int]
--     y = 'review' (injectN \@[3, 1] \@_ \@[Bool, Int, Char, String] Proxy) x -- 'diversifyN'
-- y \`shouldBe` pick (5 :: Int) :: 'Which' '[Bool, Int, Char, String]
-- let y' = 'preview' ('injectN' @[3, 1] \@[String, Int] Proxy) y -- 'reinterpertN''
-- y' \`shouldBe` Just ('pick' (5 :: Int)) :: Maybe ('Which' '[String, Int])
-- @
injectN
    :: forall indices branch tree proxy.
       ( DiversifyN indices tree branch
       , ReinterpretN indices branch tree
       )
    => proxy indices -> Prism' (Which tree) (Which branch)
injectN p = prism' (diversifyN p) (reinterpretN p)
