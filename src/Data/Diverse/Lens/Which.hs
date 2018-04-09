{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Lens.Which (
      -- * Single type
      -- ** Prism
      AsFacet(..)
    , MatchingFacet(..)
    , AsFacetL(..)
    , MatchingFacetL(..)
    , AsFacetTag(..)
    , MatchingFacetTag(..)
    -- , genericFacetTag
    , AsFacetN(..)
    , MatchingFacetN(..)

      -- * Multiple types
      -- ** Prism
    , Inject
    , inject
    -- , MatchingInject(..)
    , InjectL
    , injectL
    -- , MatchingInjectL(..)
    , InjectN
    , injectN
    ) where

import Control.Lens
import Data.Diverse.TypeLevel
import Data.Diverse.Which
import Data.Generics.Sum
import Data.Kind
import Data.Tagged
import Data.Void
import GHC.TypeLits

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
class AsFacet a s where
    facet :: Prism' s a

    -- | Make it easy to create an instance of 'AsFacet' using 'Data.Generics.Sum.Typed'
    default facet :: (AsType a s) => Prism' s a
    facet = _Typed

instance AsFacet Void (Which '[]) where
    facet = prism' absurd impossible

instance AsFacet (Which '[]) Void where
    facet = prism' impossible absurd

instance AsFacet x x where
    facet = id

instance UniqueMember x xs => AsFacet x (Which xs) where
    facet = prism' pick trial'

class AsFacet a s => MatchingFacet a s t | s a -> t where
    -- | Unfortunately, polymorphic @Prism s t a b@ cannot be used as it can only be created with:
    -- @
    -- (UniqueMember x xs, UniqueMember y ys, ys ~ Remove x xs)
    -- => prism (pick :: y -> Which ys) (trial :: Which xs -> Either (Which ys) x)
    -- @
    -- This above causes problems when used monomorphically with @s ~ t@ and @x ~ y@ since
    -- @xs@ cannot equal @ys ~ Remove x x@.
    --
    -- What is desired is:
    -- (UniqueMember x xs, ys ~ Remove x xs)
    -- => prism_ (pick :: x -> Which xs) (trial :: Which xs -> Either (Which ys) x)
    --
    -- So we expose the polymorphic 'Control.Lens.Prism.matching' explicitly.
    matchingFacet :: s -> Either t a

instance (UniqueMember x xs, ys ~ Remove x xs) => MatchingFacet x (Which xs) (Which ys) where
    matchingFacet = trial

-- | 'pickL' ('review' 'facetL') and 'trialL'' ('preview' 'facetL'') in 'Prism'' form.
--
-- @
-- let y = 'review' 'facetL' \@Bar (Tagged (5 :: Int)) :: Which '[Tagged Foo Bool, Tagged Bar Int, Char, Bool, Char]
--     x = 'preview' 'facetL' \@Bar y
-- x \`shouldBe` (Just (Tagged 5))
-- @
class AsFacetL (l :: k) a s | s l -> a where
    facetL :: Prism' s a

instance (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => AsFacetL l x (Which xs) where
    facetL = prism' (pickL @l) (trialL' @l)

-- | Labelled version of 'MatchingFacet'
class AsFacetL l a s => MatchingFacetL l a s t | s a -> t where
    matchingFacetL :: s -> Either t a

instance (UniqueLabelMember l xs, x ~ KindAtLabel l xs, ys ~ Remove x xs)
  => MatchingFacetL l x (Which xs) (Which ys) where
    matchingFacetL = trialL @l

-- | Variation of 'grabL' specialized to 'Tagged' which automatically tags and untags the field.
-- A default implementation using generics is not provided as it make GHC think that @l@ must be type @Symbol@
-- when @l@ can actually be any kind.
-- Create instances of 'AsFacetTag'' using "Data.Generics.Sum.Constructors" as follows:
-- @
-- instance AsConstructor' l Foo Foo a a => AsFacetTag l a Foo where
--     facetTag = _Ctor @l
-- @
class AsFacetTag (l :: k) a s | s l -> a where
    facetTag :: Prism' s a

-- -- | Make it easy to create an instance of 'AsFacetTag' using 'Data.Generics.Sum.Constructors'
-- -- NB. This is not a default signature for AsFacetTag, as this makes GHC think that l must be type 'Symbol', when actually @l@ can be any kind @k@
-- genericFacetTag :: forall l a s proxy. (AsConstructor l s s a a) => Prism' s a
-- genericFacetTag = _Ctor @l

instance (UniqueLabelMember l xs, Tagged l x ~ KindAtLabel l xs) => AsFacetTag l x (Which xs) where
    facetTag = prism' (pickTag @l) (trialTag' @l)

-- | Untagged version of 'MatchingFacet'
class AsFacetTag l a s => MatchingFacetTag l a s t | l s a -> t where
    matchingFacetTag :: s -> Either t a

instance (UniqueLabelMember l xs, Tagged l x ~ KindAtLabel l xs, ys ~ Remove (Tagged l x) xs)
  => MatchingFacetTag l x (Which xs) (Which ys) where
    matchingFacetTag = trialTag @l

-- | 'pickN' ('review' 'facetN') and 'trialN' ('preview' 'facetN') in 'Prism'' form.
--
-- @
-- 'facetN' p = 'prism'' ('pickN' p) ('trialN'' p)
-- @
--
-- @
-- let y = 'review' ('facetN' \@4) (5 :: Int) :: 'Which' '[Bool, Int, Char, Bool, Int, Char] -- 'pickN'
--     x = 'preview' ('facetN' \@4) y -- 'trialN'
-- x \`shouldBe` (Just 5)
-- @
class AsFacetN (n :: Nat) a s | s n -> a where
    facetN :: Prism' s a

instance (MemberAt n x xs) => AsFacetN n x (Which xs) where
    facetN = prism' (pickN @n) (trialN' @n)

-- | Nat indexed version of 'MatchingFacet'
class AsFacetN n a s => MatchingFacetN n a s t | s a -> t where
    matchingFacetN :: s -> Either t a

instance (MemberAt n x xs, ys ~ RemoveIndex n xs)
  => MatchingFacetN n x (Which xs) (Which ys) where
    matchingFacetN = trialN @n

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'inject'
type Inject (branch :: [Type]) (tree :: [Type]) = ( Diversify branch tree
                                                  , Reinterpret' branch tree
                                                  )

-- | 'diversify' ('review' 'inject') and 'reinterpret'' ('preview' 'inject') in 'Prism'' form.
--
-- @
-- let x = 'pick' (5 :: Int) :: 'Which' '[String, Int]
--     y = 'review' ('inject' \@_  @_ \@[Bool, Int, Char, String]) x -- 'diversify'
-- y \`shouldBe` pick (5 :: Int) :: 'Which' '[Bool, Int, Char, String]
-- let y' = 'preview' ('inject' \@_ \@[String, Int]) y -- 'reinterpret'
-- y' \`shouldBe` Just (pick (5 :: Int)) :: Maybe ('Which' '[String, Int])
-- @
inject :: forall branch tree. (Inject branch tree) => Prism' (Which tree) (Which branch)
inject = prism' diversify reinterpret'

-- | A friendlier constraint synonym for 'injectL'
type InjectL (ls :: [k]) (branch :: [Type]) (tree :: [Type]) =
    ( Diversify branch tree
    , Reinterpret' branch tree
    , branch ~ KindsAtLabels ls tree
    , UniqueLabels ls tree
    , IsDistinct ls
    )

-- | 'diversifyL' ('review' 'injectL') and 'reinterpretL'' ('preview' 'injectL') in 'Prism'' form.
--
-- @
-- let t = 'pick' \@[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool] (5 :: Tagged Bar Int)
--     b = 'pick' \@'[Tagged Foo Bool, Tagged Bar Int] (5 :: Tagged Bar Int)
--     t' = 'review' ('injectL' \@_ \@[Foo, Bar] \@_ \@[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool]) b
--     b' = 'preview' ('injectL' \@_ \@[Foo, Bar]) t'
-- t \`shouldBe` t'
-- b' \`shouldBe` Just b
-- @
injectL :: forall ls branch tree. InjectL ls branch tree => Prism' (Which tree) (Which branch)
injectL = prism' (diversifyL @ls) (reinterpretL' @ls)

-- | A friendlier constraint synonym for 'injectN'
type InjectN (ns :: [Nat]) (branch :: [Type]) (tree :: [Type]) =
    ( DiversifyN ns branch tree
    , ReinterpretN' ns branch tree
    )

-- | 'diversifyN' ('review' 'injectN') and 'reinterpretN'' ('preview' 'injectN') in 'Prism'' form.
--
-- @
-- let x = 'pick' (5 :: Int) :: 'Which' '[String, Int]
--     y = 'review' (injectN \@_ \@[3, 1] \@_ \@[Bool, Int, Char, String]) x -- 'diversifyN'
-- y \`shouldBe` pick (5 :: Int) :: 'Which' '[Bool, Int, Char, String]
-- let y' = 'preview' ('injectN' \@_ @[3, 1] \@[String, Int]) y -- 'reinterpertN''
-- y' \`shouldBe` Just ('pick' (5 :: Int)) :: Maybe ('Which' '[String, Int])
-- @

injectN :: forall ns branch tree. InjectN ns branch tree => Prism' (Which tree) (Which branch)
injectN = prism' (diversifyN @ns) (reinterpretN' @ns)
