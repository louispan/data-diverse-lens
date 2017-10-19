{-# LANGUAGE DataKinds #-}
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

      -- * Profunctor Choice
    , faceted
    , faceted'
    , injected
    ) where

import Control.Lens
import Data.Diverse.Which
import Data.Diverse.TypeLevel

-----------------------------------------------------------------

-- | 'pick' ('review' 'facet') and 'trial' ('preview' 'facet') in 'Prism'' form.
--
-- @
-- 'facet' = 'prism'' 'pick' ('trial'')
-- @
--
-- @
-- let y = 'review' ('facet' \@Int) (5 :: Int) :: 'Which' '[Bool, Int, Char, Bool, Char] -- 'pick'
--     x = 'preview' ('facet' \@Int) y -- 'trial'
-- x \`shouldBe` (Just 5)
-- @
facet :: forall x xs. (UniqueMember x xs) => Prism' (Which xs) x
facet = prism' pick trial'

-- | 'pickL' ('review' 'facetL') and 'trialL'' ('preview' 'facetL'') in 'Prism'' form.
--
-- @
-- let y = 'review' ('facetL' \@Bar Proxy) (Tagged (5 :: Int)) :: Which '[Tagged Foo Bool, Tagged Bar Int, Char, Bool, Char]
--     x = 'preview' ('facetL' \@Bar Proxy) y
-- x \`shouldBe` (Just (Tagged 5))
-- @
facetL :: forall l xs proxy x. (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => proxy l -> Prism' (Which xs) x
facetL p = prism' (pickL p) (trialL' p)

-- | 'pickN' ('review' 'facetN') and 'trialN' ('preview' 'facetN') in 'Prism'' form.
--
-- @
-- 'facetN' p = 'prism'' ('pickN' p) ('trialN'' p)
-- @
--
-- @
-- let y = 'review' ('facetN' (Proxy \@4)) (5 :: Int) :: 'Which' '[Bool, Int, Char, Bool, Int, Char] -- 'pickN'
--     x = 'preview' ('facetN' (Proxy \@4)) y -- 'trialN'
-- x \`shouldBe` (Just 5)
-- @
facetN :: forall n xs proxy x. (MemberAt n x xs) => proxy n -> Prism' (Which xs) x
facetN p = prism' (pickN p) (trialN' p)

------------------------------------------------------------------


-- | 'diversify' ('review' 'inject') and 'reinterpret'' ('preview' 'inject') in 'Prism'' form.
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
       ( Diversify branch tree
       , Reinterpret' branch tree
       )
    => Prism' (Which tree) (Which branch)
inject = prism' diversify reinterpret'

-- | 'diversifyL' ('review' 'injectL') and 'reinterpretL'' ('preview' 'injectL') in 'Prism'' form.
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
       ( Diversify branch tree
       , Reinterpret' branch tree
       , branch ~ KindsAtLabels ls tree
       , UniqueLabels ls tree
       , IsDistinct ls
       )
    => proxy ls -> Prism' (Which tree) (Which branch)
injectL p = prism' (diversifyL p) (reinterpretL' p)

-- | 'diversifyN' ('review' 'injectN') and 'reinterpretN'' ('preview' 'injectN') in 'Prism'' form.
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
       ( DiversifyN indices branch tree
       , ReinterpretN' indices branch tree
       )
    => proxy indices -> Prism' (Which tree) (Which branch)
injectN p = prism' (diversifyN p) (reinterpretN' p)

-- | Like 'Choice' or 'ArrowChoice' but lifting into 'Which'
faceted
    :: ( Profunctor w
       , Choice w
       , UniqueMember a a'
       , UniqueMember b b'
       , Diversify (Complement a' '[a]) b'
       )
    => w a b -> w (Which a') (Which b')
faceted w = dimap trial (either diversify pick) (right' w)

-- | Like 'Choice' or 'ArrowChoice' but lifting into 'Which' of one type
faceted' :: (Profunctor w, Choice w) => w a b -> w (Which '[a]) (Which '[b])
faceted' w = dimap trial (either impossible pick) (right' w)

-- | Like 'Choice' or 'ArrowChoice' but lifting from 'Which' into another type of 'Which'
injected
    :: ( Profunctor w
       , Choice w
       , Reinterpret a a'
       , Diversify b (AppendUnique b (Complement a' a))
       , Diversify (Complement a' a) (AppendUnique b (Complement a' a))
       -- all of @a@ is used, ie @a'@ is not a subset of @a@
       -- this ensures that all of '[a] is used to avoid surprises (eg. of noop behaviour)
       , Complement a a' ~ '[]
       )
    => proxy a'
    -> w (Which a) (Which b)
    -> w (Which a') (Which (AppendUnique b (Complement a' a)))
injected _ w = dimap reinterpret (either diversify diversify) (right' w)
