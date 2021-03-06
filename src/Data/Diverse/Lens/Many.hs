{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Lens.Many (
      -- * Isomorphism
      _Many
    , _Many'

    -- * Single field
    -- ** Lens for a single field
    , Has(..)
    , hasTag
    , hadTag
    , Had(..)
    , HasL(..)
    , HadL(..)
    -- , genericItemTag
    , HasN(..)
    , HadN(..)

    -- * Multiple fields
    -- ** Lens for multiple fields
    , Project
    , project
    , Project'
    , project'
    , ProjectL
    , projectL
    , ProjectL'
    , projectL'
    , ProjectN
    , projectN
    , ProjectN'
    , projectN'
    ) where

import Control.Lens
import Data.Diverse.Many
import Data.Diverse.TypeLevel
-- import Data.Generics.Product
import Data.Has
import Data.Kind
import Data.Tagged
import GHC.TypeLits

-- | @_Many = iso fromMany toMany@
_Many :: IsMany t xs a => Iso' (Many xs) (t xs a)
_Many = iso fromMany toMany

-- | @_Many' = iso fromMany' toMany'@
_Many' :: IsMany Tagged xs a => Iso' (Many xs) a
_Many' = iso fromMany' toMany'

-----------------------------------------------------------------------

-- -- | Convient name for 'hasLens' to be consistent with 'Had' typeclass.
-- --
-- -- @
-- -- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- -- x '^.' 'has'' \@Int \`shouldBe` 5
-- -- (x '&' 'has'' \@Int .~ 6) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- -- @
-- has' :: Has a s => Lens' s a
-- has' = hasLens

instance UniqueMember x xs => Has x (Many xs) where
    hasLens = lens grab replace'

-- | Polymorphic version of 'has''
class (Has a s, Replaced a a s ~ s) => Had a s where
    type Replaced a b s
    hadLens :: Lens s (Replaced a b s) a b

instance (UniqueMember x xs) => Had x (Many xs) where
    type Replaced x b (Many xs) = Many (Replace x b xs)
    hadLens = lens grab (replace @x)

hasTag :: forall l a s. Has (Tagged l a) s => Lens' s a
hasTag = hasLens @(Tagged l a) . iso unTagged Tagged

hadTag :: forall l a b s. Had (Tagged l a) s
    => Lens s (Replaced (Tagged l a) (Tagged l b) s) a b
hadTag = hadLens @(Tagged l a) . iso unTagged (Tagged @l)

-- | 'grabL' ('view' 'hasL') and 'replaceL' ('set' 'hasL') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' 'nil'
-- x '^.' 'hasL'' \@Foo \`shouldBe` Tagged \@Foo False
-- (x '&' 'hasL'' \@Foo '.~' Tagged \@Foo True) \`shouldBe` (5 :: Int) './' Tagged \@Foo True './' Tagged \@Bar \'X' './' 'nil'
-- @
--
-- A default implementation using generics is not provided as it make GHC think that @l@ must be type @Symbol@
-- when @l@ can actually be any kind.
-- Create instances of 'HasL' using "Data.Generics.Product.Fields" as follows:
-- @
-- instance HasField' l Foo a => hasL' l a Foo where
--     hasL' = field @l
-- default hasL' :: forall (l :: Symbol) a s. (HasField' l s a) => Lens' s a
-- hasL' = field @l
class HasL (l :: k) a s | s l -> a where
    hasL :: Lens' s a

instance (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => HasL l x (Many xs) where
    hasL = lens (grabL @l) (replaceL' @l)

-- | Polymorphic version of 'hasL''
--
-- @
-- let x = (5 :: Int) './' Tagged @Foo False './' Tagged \@Bar \'X' './' 'nil'
-- (x '&' 'hasL' \@Foo '.~' \"foo") \`shouldBe` (5 :: Int) './' \"foo" './' Tagged \@Bar \'X' './' 'nil'
-- @
class (HasL (l :: k) a s, ReplacedL l a a s ~ s) => HadL (l :: k) a s | s l -> a where
    type ReplacedL l a b s
    hadL :: Lens s (ReplacedL l a b s) a b

instance (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => HadL l x (Many xs) where
    type ReplacedL l x b (Many xs) = Many (Replace (KindAtLabel l xs) b xs)
    hadL = lens (grabL @l) (replaceL @l)

-- | 'grabN' ('view' 'has') and 'replaceN'' ('set' 'has'') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' ./ nil
-- x '^.' 'hasN'' \@0 \`shouldBe` 5
-- (x '&' 'hasN'' \@0 '.~' 6) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
-- @
class HasN (n :: Nat) a s | s n -> a where
    hasN :: Lens' s a

instance (MemberAt n x xs) => HasN n x (Many xs) where
    hasN = lens (grabN @n) (replaceN' @n)

-- | Polymorphic version of 'hasN''
class (HasN (n :: Nat) a s, ReplacedN n a a s ~ s) => HadN (n :: Nat) a s | s n -> a where
    type ReplacedN n a b s
    hadN :: Lens s (ReplacedN n a b s) a b

    -- -- | Make it easy to create an instance of 'hasN' using 'Data.Generics.Product.Positions'
    -- default hasN :: (HasPosition n s (ReplacedN n a b s) a b) => Lens s (ReplacedN n a b s) a b
    -- hasN = position @n

instance (MemberAt n x xs)
  => HadN n x (Many xs) where
    type ReplacedN n x b (Many xs) = Many (ReplaceIndex n x b xs)
    hadN = lens (grabN @n) (replaceN @n)

-----------------------------------------------------------------------

-- | A friendlier constraint synonym for 'project''.
type Project' (smaller :: [Type]) (larger :: [Type]) = (Select smaller larger, Amend' smaller larger)

-- | 'select' ('view' 'project') and 'amend' ('set' 'project') in 'Lens'' form.
--
-- @
-- 'project' = 'lens' 'select' 'amend'
-- @
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- x '^.' ('project'' \@_ \@'[Int, Maybe Char]) \`shouldBe` (5 :: Int) './' Just \'O' './' 'nil'
-- (x '&' ('project'' \@_ \@'[Int, Maybe Char]) '.~' ((6 :: Int) './' Just 'P' './' 'nil')) \`shouldBe`
--     (6 :: Int) './' False './' \'X' './' Just \'P' './' 'nil'
-- @
project' :: forall smaller larger. Project' smaller larger => Lens' (Many larger) (Many smaller)
project' = lens select amend'

-- | A friendlier constraint synonym for 'project'.
type Project (smaller :: [Type]) (smaller' :: [Type]) (larger :: [Type]) (larger' :: [Type]) =
    ( Select smaller larger
    , Amend smaller smaller' larger
    , larger' ~ Replaces smaller smaller' larger
    )

-- | Polymorphic version of project'
project :: forall smaller smaller' larger larger'. Project smaller smaller' larger larger'
    => Lens (Many larger) (Many larger') (Many smaller) (Many smaller')
project = lens select (amend @smaller @smaller')

-- | A friendlier constraint synonym for 'projectL''.
type ProjectL' (ls :: [k]) (smaller :: [Type]) (larger :: [Type]) =
    ( Select smaller larger
    , Amend' smaller larger
    , smaller ~ KindsAtLabels ls larger
    , IsDistinct ls
    , UniqueLabels ls larger
    )

-- | 'selectL' ('view' 'projectL') and 'amendL' ('set' 'projectL') in 'Lens'' form.
--
-- @
-- let x = False './' Tagged \@\"Hi" (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'O' './' 'nil'
-- x '^.' ('projectL'' \@'[Foo, Bar] \`shouldBe` Tagged \@Foo False './' Tagged \@Bar \'X' './' nil
-- (x '&' ('projectL'' \@'[\"Hi", \"Bye"] '.~' (Tagged \@\"Hi" (6 :: Int) './' Tagged \@\"Bye" \'P' './' nil)) '`shouldBe`
--     False './' Tagged \@\"Hi" (6 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'P' './' 'nil'
-- @
projectL' :: forall ls smaller larger. ProjectL' ls smaller larger => Lens' (Many larger) (Many smaller)
projectL' = lens (selectL @ls) (amendL' @ls)

-- | A friendlier constraint synonym for 'projectL'.
type ProjectL (ls :: [k]) (smaller :: [Type]) (smaller' :: [Type]) (larger :: [Type]) (larger' :: [Type]) =
    ( Select smaller larger
    , Amend smaller smaller' larger
    , smaller ~ KindsAtLabels ls larger
    , IsDistinct ls
    , UniqueLabels ls larger
    , larger' ~ Replaces smaller smaller' larger
    )

-- | Polymorphic version of 'projectL''
--
-- @
-- let x = False './' Tagged \@\"Hi" (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'O' './' 'nil'
-- (x '&' ('projectL' \@'[\"Hi", \"Bye"] '.~' (True './' Tagged \@\"Changed" False './' 'nil')) \`shouldBe`
--     False './' True './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Changed" False './' 'nil'
-- @
projectL :: forall ls smaller smaller' larger larger'. ProjectL ls smaller smaller' larger larger'
  => Lens (Many larger) (Many larger') (Many smaller) (Many smaller')
projectL = lens (selectL @ls) (amendL @ls)

-- | A friendlier constraint synonym for 'projectN''.
type ProjectN' (ns :: [Nat]) (smaller :: [Type]) (larger :: [Type]) =
    (SelectN ns smaller larger, AmendN' ns smaller larger)

-- | 'selectN' ('view' 'projectN') and 'amendN' ('set' 'projectN') in 'Lens'' form.
--
-- @
-- 'projectN' = 'lens' 'selectN' 'amendN'
-- @
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
-- x '^.' 'projectN' \@_ \@'[5, 4, 0] \`shouldBe` Just \'A' './' (6 :: Int) './' (5 ::Int) './' 'nil'
-- (x '&' 'projectN' \@_ \@'[5, 4, 0] '.~' (Just \'B' './' (8 :: Int) './' (4 ::Int) './' nil)) \`shouldBe`
--     (4 :: Int) './' False './' \'X' './' Just \'O' './' (8 :: Int) './' Just \'B' './' 'nil'
-- @
projectN' :: forall ns smaller larger. ProjectN' ns smaller larger => Lens' (Many larger) (Many smaller)
projectN' = lens (selectN @ns) (amendN' @ns)

-- | A friendlier constraint synonym for 'projectN'.
type ProjectN (ns :: [Nat]) (smaller :: [Type]) (smaller' :: [Type]) (larger :: [Type]) (larger' :: [Type]) =
    (SelectN ns smaller larger, AmendN ns smaller smaller' larger, larger' ~ ReplacesIndex ns smaller' larger)

-- | Polymorphic version of 'projectN''
projectN :: forall ns smaller smaller' larger larger'. ProjectN ns smaller smaller' larger larger'
    => Lens (Many larger) (Many larger') (Many smaller) (Many smaller')
projectN = lens (selectN @ns) (amendN @ns)
