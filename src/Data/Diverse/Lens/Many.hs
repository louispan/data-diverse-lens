{-# LANGUAGE AllowAmbiguousTypes #-}
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

module Data.Diverse.Lens.Many (
      -- * Isomorphism
      _Many
    , _Many'

    -- * Single field
    -- ** Lens for a single field
    , HasItem(..)
    , item'
    , itemL
    , itemL'
    , itemTag
    , itemTag'
    , genericItemTag
    , itemN
    , itemN'

    -- * Multiple fields
    -- ** Lens for multiple fields
    , project
    , project'
    , projectL
    , projectL'
    , projectN
    , projectN'
    ) where

import Control.Lens
import Data.Tagged
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import Data.Generics.Product
import GHC.TypeLits

-- | @_Many = iso fromMany toMany@
_Many :: IsMany t xs a => Iso' (Many xs) (t xs a)
_Many = iso fromMany toMany

-- | @_Many' = iso fromMany' toMany'@
_Many' :: IsMany Tagged xs a => Iso' (Many xs) a
_Many' = iso fromMany' toMany'

-----------------------------------------------------------------------

-- | 'fetch' ('view' 'item') and 'replace'' ('set' 'item'') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- x '^.' 'item'' \@Int \`shouldBe` 5
-- (x '&' 'item'' \@Int .~ 6) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- @
class HasItem' a s where
    item' :: Lens' s a

    -- | Make it easy to create an instance of 'item' using 'Data.Generics.Product.Typed'
    default item' :: (HasType a s) => Lens' s a
    item' = typed

instance UniqueMember x xs => HasItem' x (Many xs) where
    item' = lens fetch replace'

-- | Polymorphic version of 'item''
class HasItem a b s t | s a b -> t, t a b -> s where
    item :: Lens s t a b

instance (UniqueMember x xs, ys ~ Replace x y xs) => HasItem x y (Many xs) (Many ys) where
    item = lens fetch (replace @x @y)

-- | 'fetchL' ('view' 'itemL') and 'replaceL' ('set' 'itemL') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' 'nil'
-- x '^.' 'itemL'' \@Foo Proxy \`shouldBe` Tagged \@Foo False
-- (x '&' 'itemL'' \@Foo Proxy '.~' Tagged \@Foo True) \`shouldBe` (5 :: Int) './' Tagged \@Foo True './' Tagged \@Bar \'X' './' 'nil'
-- @
class HasItemL' (l :: k) a s | s l -> a where
    itemL' :: Lens' s a

instance (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => HasItemL' l x (Many xs) where
    itemL' = lens (fetchL @l) (replaceL' @l)

-- | Polymorphic version of 'itemL''
--
-- @
-- let x = (5 :: Int) './' Tagged @Foo False './' Tagged \@Bar \'X' './' 'nil'
-- (x '&' 'itemL' \@Foo Proxy '.~' \"foo") \`shouldBe` (5 :: Int) './' \"foo" './' Tagged \@Bar \'X' './' 'nil'
-- @
class HasItemL (l :: k) a b s t | s l -> a, t l -> b, s l b -> t, t l a -> s where
    itemL :: Lens s t a b

instance (UniqueLabelMember l xs, x ~ KindAtLabel l xs, ys ~ Replace x y xs)
  => HasItemL l x y (Many xs) (Many ys) where
    itemL = lens (fetchL @l) (replaceL @l)

-- | Variation of 'itemL'' that automatically tags and untags the field.
class HasItemTag' (l :: k) a s | s l -> a where
    itemTag' :: Lens' s a

instance (UniqueLabelMember l xs, Tagged l x ~ KindAtLabel l xs) => HasItemTag' l x (Many xs) where
    itemTag' = lens (fetchTag @l) (replaceTag' @l)

-- | Variation of 'itemL' that automatically tags and untags the field.
class HasItemTag (l :: k) a b s t | s l -> a, t l -> b, s l b -> t, t l a -> s where
    itemTag :: Lens s t a b

-- | Make it easy to create an instance of 'itemTag' using 'Data.Generics.Product.Fields'
-- NB. This is not a default signature for HasItemTag, as this makes GHC think that l must be type 'Symbol'
genericItemTag :: forall l a b s t. (HasField l s t a b) => Lens s t a b
genericItemTag = field @l

instance (UniqueLabelMember l xs, Tagged l x ~ KindAtLabel l xs, ys ~ Replace (Tagged l x) (Tagged l y) xs)
  => HasItemTag l x y (Many xs) (Many ys) where
    itemTag = lens (fetchTag @l) (replaceTag @l)

-- | 'fetchN' ('view' 'item') and 'replaceN'' ('set' 'item'') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' ./ nil
-- x '^.' 'itemN'' (Proxy \@0) \`shouldBe` 5
-- (x '&' 'itemN'' (Proxy @0) '.~' 6) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
-- @
class HasItemN' (n :: Nat) a s | s n -> a where
    itemN' :: Lens' s a

instance (MemberAt n x xs) => HasItemN' n x (Many xs) where
    itemN' = lens (fetchN @n) (replaceN' @n)

-- | Polymorphic version of 'itemN''
class HasItemN (n :: Nat) a b s t | s n -> a, t n -> b, s n b -> t, t n a -> s where
    itemN :: Lens s t a b

    -- | Make it easy to create an instance of 'itemN' using 'Data.Generics.Product.Positions'
    default itemN :: (HasPosition n s t a b) => Lens s t a b
    itemN = position @n

instance (MemberAt n x xs, ys ~ ReplaceIndex n y xs)
  => HasItemN n x y (Many xs) (Many ys) where
    itemN = lens (fetchN @n) (replaceN @n)

-----------------------------------------------------------------------

-- type family Projected (xs :: k) s
-- type instance Projected xs (Many _) = Many xs

-- | 'select' ('view' 'project') and 'amend' ('set' 'project') in 'Lens'' form.
--
-- @
-- 'project' = 'lens' 'select' 'amend'
-- @
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- x '^.' ('project'' \@'[Int, Maybe Char]) \`shouldBe` (5 :: Int) './' Just \'O' './' 'nil'
-- (x '&' ('project'' \@'[Int, Maybe Char]) '.~' ((6 :: Int) './' Just 'P' './' 'nil')) \`shouldBe`
--     (6 :: Int) './' False './' \'X' './' Just \'P' './' 'nil'
-- @
class HasProject' (as :: k) (ss :: k) a s | a -> as, s -> ss, s as -> a, a ss -> s where
    project' :: Lens' s a

    -- | Make it easy to create an instance of 'project' using 'Data.Generics.Product.Subtype'
    default project' :: (Subtype a s) => Lens' s a
    project' = super

instance (Select smaller larger, Amend' smaller larger)
  => HasProject' smaller larger (Many smaller) (Many larger) where
    project' = lens select amend'

-- | Polymorphic version of project'
class HasProject (as :: k) (bs :: k) (ss :: k) (ts :: k) a b s t
        | a -> as, b -> bs, s -> ss, t -> ts
        , b as -> a, s as -> a, t as -> a
        , a bs -> b, s bs -> b, t bs -> b
        , a ss -> s, b ss -> s, t ss -> s
        , a ts -> t, b ts -> t, s ts -> t
        , s a b -> t, t a b -> s where
    project :: Lens s t a b

instance (Select smaller larger, Amend smaller smaller' larger, larger' ~ Replaces smaller smaller' larger)
  => HasProject smaller smaller' larger larger' (Many smaller) (Many smaller') (Many larger) (Many larger') where
    project = lens select (amend @smaller @smaller')

-- | 'selectL' ('view' 'projectL') and 'amendL' ('set' 'projectL') in 'Lens'' form.
--
-- @
-- let x = False './' Tagged \@\"Hi" (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'O' './' 'nil'
-- x '^.' ('projectL'' \@'[Foo, Bar] Proxy) \`shouldBe` Tagged \@Foo False './' Tagged \@Bar \'X' './' nil
-- (x '&' ('projectL'' \@'[\"Hi", \"Bye"] Proxy) '.~' (Tagged \@\"Hi" (6 :: Int) './' Tagged \@\"Bye" \'P' './' nil)) '`shouldBe`
--     False './' Tagged \@\"Hi" (6 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'P' './' 'nil'
-- @
class HasProjectL' (ls :: k1) (as :: k) (ss :: k) a s | a -> as, s -> ss, s as -> a, a ss -> s, s ls -> as where
    projectL' ::  Lens' s a

instance ( Select smaller larger
         , Amend' smaller larger
         , smaller ~ KindsAtLabels ls larger
         , IsDistinct ls
         , UniqueLabels ls larger) => HasProjectL' ls smaller larger (Many smaller) (Many larger) where
    projectL' = lens (selectL @ls) (amendL' @ls)

-- | Polymorphic version of 'projectL''
--
-- @
-- let x = False './' Tagged \@\"Hi" (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'O' './' 'nil'
-- (x '&' ('projectL' \@'[\"Hi", \"Bye"] Proxy) '.~' (True './' Tagged \@\"Changed" False './' 'nil')) \`shouldBe`
--     False './' True './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Changed" False './' 'nil'
-- @
class HasProjectL (ls :: k1) (as :: k) (bs :: k) (ss :: k) (ts :: k) a b s t
        | a -> as, b -> bs, s -> ss, t -> ts
        , b as -> a, s as -> a, t as -> a
        , a bs -> b, s bs -> b, t bs -> b
        , a ss -> s, b ss -> s, t ss -> s
        , a ts -> t, b ts -> t, s ts -> t
        , s ls -> as, t ls -> bs, s ls b -> t, t ls a -> s where
    projectL :: Lens s t a b

instance ( Select smaller larger
         , Amend smaller smaller' larger
         , smaller ~ KindsAtLabels ls larger
         , IsDistinct ls
         , UniqueLabels ls larger
         , larger' ~ Replaces smaller smaller' larger)
  => HasProjectL ls smaller smaller' larger larger' (Many smaller) (Many smaller') (Many larger) (Many larger') where
    projectL = lens (selectL @ls) (amendL @ls)

-- | 'selectN' ('view' 'projectN') and 'amendN' ('set' 'projectN') in 'Lens'' form.
--
-- @
-- 'projectN' = 'lens' 'selectN' 'amendN'
-- @
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
-- x '^.' ('projectN' \@'[5, 4, 0] Proxy) \`shouldBe` Just \'A' './' (6 :: Int) './' (5 ::Int) './' 'nil'
-- (x '&' ('projectN' \@'[5, 4, 0] Proxy) '.~' (Just \'B' './' (8 :: Int) './' (4 ::Int) './' nil)) \`shouldBe`
--     (4 :: Int) './' False './' \'X' './' Just \'O' './' (8 :: Int) './' Just \'B' './' 'nil'
-- @
class HasProjectN' (ns :: [Nat]) (as :: k) (ss :: k) a s | a -> as, s -> ss, s as -> a, a ss -> s, s ns -> as where
    projectN' :: Lens' s a

instance (SelectN ns smaller larger, AmendN' ns smaller larger)
  => HasProjectN' ns smaller larger (Many smaller) (Many larger) where
    projectN' = lens (selectN @ns) (amendN' @ns)

-- | Polymorphic version of 'projectN''
class HasProjectN (ns :: [Nat]) (as :: k) (bs :: k) (ss :: k) (ts :: k) a b s t
        | a -> as, b -> bs, s -> ss, t -> ts
        , b as -> a, s as -> a, t as -> a
        , a bs -> b, s bs -> b, t bs -> b
        , a ss -> s, b ss -> s, t ss -> s
        , a ts -> t, b ts -> t, s ts -> t
        , s ns -> as, t ns -> bs, s ns b -> t, t ns a -> s where
    projectN :: Lens s t a b

instance (SelectN ns smaller larger, AmendN ns smaller smaller' larger, larger' ~ ReplacesIndex ns smaller' larger)
  => HasProjectN ns smaller smaller' larger larger' (Many smaller) (Many smaller') (Many larger) (Many larger') where
    projectN = lens (selectN @ns) (amendN @ns)
