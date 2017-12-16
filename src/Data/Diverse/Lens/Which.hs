{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Lens.Which (
      -- * Single type
      -- ** Prism
      facet
    , facetL
    , facetTag
    , genericFacetTag
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
import Data.Generics.Sum
import Data.Tagged
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

instance UniqueMember x xs => AsFacet x (Which xs) where
    facet = prism' pick trial'

-- | 'pickL' ('review' 'facetL') and 'trialL'' ('preview' 'facetL'') in 'Prism'' form.
--
-- @
-- let y = 'review' ('facetL' \@Bar Proxy) (Tagged (5 :: Int)) :: Which '[Tagged Foo Bool, Tagged Bar Int, Char, Bool, Char]
--     x = 'preview' ('facetL' \@Bar Proxy) y
-- x \`shouldBe` (Just (Tagged 5))
-- @
class AsFacetL (l :: k) a s | s l -> a where
    facetL :: proxy l -> Prism' s a

instance (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => AsFacetL l x (Which xs) where
    facetL p = prism' (pickL p) (trialL' p)

-- | Variation of 'fetchL' specialized to 'Tagged' which automatically tags and untags the field.
class AsFacetTag (l :: k) a s | s l -> a where
    facetTag :: proxy l -> Prism' s a

-- | Make it easy to create an instance of 'AsFacetTag' using 'Data.Generics.Sum.Constructors'
-- NB. This is not a default signature for AsFacetTag, as this makes GHC think that l must be type 'Symbol', when actually @l@ can be any kind @k@
genericFacetTag :: forall l a s proxy. (AsConstructor l s s a a) => proxy l -> Prism' s a
genericFacetTag _ = _Ctor @l

instance (UniqueLabelMember l xs, Tagged l x ~ KindAtLabel l xs) => AsFacetTag l x (Which xs) where
    facetTag p = prism' (pickTag p) (trialTag' p)

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
class AsFacetN (n :: Nat) a s | s n -> a where
    facetN :: proxy n -> Prism' s a

instance (MemberAt n x xs) => AsFacetN n x (Which xs) where
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
class AsInject (as :: k) (ss :: k) a s | a -> as, s -> ss, s as -> a, a ss -> s where
    inject :: Prism' s a

instance ( Diversify branch tree
         , Reinterpret' branch tree
         ) => AsInject branch tree (Which branch) (Which tree) where
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
class AsInjectL (ls :: k1) (as :: k) (ss :: k) a s | a -> as, s -> ss, s as -> a, a ss -> s, s ls -> as where
    injectL :: proxy ls -> Prism' s a

instance ( Diversify branch tree
         , Reinterpret' branch tree
         , branch ~ KindsAtLabels ls tree
         , UniqueLabels ls tree
         , IsDistinct ls
         ) => AsInjectL ls branch tree (Which branch) (Which tree) where
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
class AsInjectN (ns :: [Nat]) (as :: k) (ss :: k) a s | a -> as, s -> ss, s as -> a, a ss -> s, s ns -> as where
    injectN :: proxy ns -> Prism' s a

instance ( DiversifyN ns branch tree
       , ReinterpretN' ns branch tree
       ) => AsInjectN ns branch tree (Which branch) (Which tree) where
    injectN p = prism' (diversifyN p) (reinterpretN' p)
