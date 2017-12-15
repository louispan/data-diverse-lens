{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Diverse.Lens.Many (
      -- * Isomorphism
      _Many
    , _Many'

    -- * Single field
    -- ** Lens for a single field
    , item
    , item'
    , itemL
    , itemL'
    , itemTag
    , itemTag'
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
import Data.Proxy

-- | @_Many = iso fromMany toMany@
_Many :: IsMany t xs a => Iso' (Many xs) (t xs a)
_Many = iso fromMany toMany

-- | @_Many' = iso fromMany' toMany'@
_Many' :: IsMany Tagged xs a => Iso' (Many xs) a
_Many' = iso fromMany' toMany'

-----------------------------------------------------------------------

-- | 'fetch' ('view' 'item') and 'replace' ('set' 'item') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- x '^.' 'item' \@Int \`shouldBe` 5
-- (x '&' 'item' \@Int .~ 6) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- @
item :: forall x xs. UniqueMember x xs => Lens' (Many xs) x
item = lens fetch replace

-- | Polymorphic version of 'item'
item' :: forall x y xs. UniqueMember x xs => Lens (Many xs) (Many (Replace x y xs)) x y
item' = lens fetch (replace' @x @y Proxy)


-- | 'fetchL' ('view' 'itemL') and 'replaceL' ('set' 'itemL') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' 'nil'
-- x '^.' 'itemL' \@Foo Proxy \`shouldBe` Tagged \@Foo False
-- (x '&' 'itemL' \@Foo Proxy '.~' Tagged \@Foo True) \`shouldBe` (5 :: Int) './' Tagged \@Foo True './' Tagged \@Bar \'X' './' 'nil'
-- @
itemL :: forall l xs proxy x. (UniqueLabelMember l xs, x ~ KindAtLabel l xs)
    => proxy l -> Lens' (Many xs) x
itemL p = lens (fetchL p) (replaceL p)

-- | Polymorphic version of 'itemL'
--
-- @
-- let x = (5 :: Int) './' Tagged @Foo False './' Tagged \@Bar \'X' './' 'nil'
-- (x '&' itemL' \@Foo Proxy '.~' \"foo") \`shouldBe` (5 :: Int) './' \"foo" './' Tagged \@Bar \'X' './' 'nil'
-- @
itemL' :: forall l y xs proxy x. (UniqueLabelMember l xs, x ~ KindAtLabel l xs)
    => proxy l -> Lens (Many xs) (Many (Replace x y xs)) x y
itemL' p = lens (fetchL p) (replaceL' p)

-- | Variation of 'itemL' that automatically tags and untags the field.
itemTag :: forall l xs proxy x. (UniqueLabelMember l xs, Tagged l x ~ KindAtLabel l xs)
    => proxy l -> Lens' (Many xs) x
itemTag p = lens (fetchTag p) (replaceTag p)

-- | Variation of 'itemL'' that automatically tags and untags the field.
itemTag' :: forall l y xs proxy x. (UniqueLabelMember l xs, Tagged l x ~ KindAtLabel l xs)
    => proxy l -> Lens (Many xs) (Many (Replace (Tagged l x) (Tagged l y) xs)) x y
itemTag' p = lens (fetchTag p) (replaceTag' p)


-- | 'fetchN' ('view' 'item') and 'replaceN' ('set' 'item') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' ./ nil
-- x '^.' 'itemN' (Proxy \@0) \`shouldBe` 5
-- (x '&' 'itemN' (Proxy @0) '.~' 6) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
-- @
itemN ::  forall n xs proxy x. MemberAt n x xs => proxy n -> Lens' (Many xs) x
itemN p = lens (fetchN p) (replaceN p)


-- | Polymorphic version of 'itemN'
itemN' ::  forall n y xs proxy x. MemberAt n x xs => proxy n -> Lens (Many xs) (Many (ReplaceIndex n y xs)) x y
itemN' p = lens (fetchN p) (replaceN' @n @y p)

-----------------------------------------------------------------------

-- | 'select' ('view' 'project') and 'amend' ('set' 'project') in 'Lens'' form.
--
-- @
-- 'project' = 'lens' 'select' 'amend'
-- @
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' 'nil'
-- x '^.' ('project' \@'[Int, Maybe Char]) \`shouldBe` (5 :: Int) './' Just \'O' './' 'nil'
-- (x '&' ('project' \@'[Int, Maybe Char]) '.~' ((6 :: Int) './' Just 'P' './' 'nil')) \`shouldBe`
--     (6 :: Int) './' False './' \'X' './' Just \'P' './' 'nil'
-- @
project
    :: forall smaller larger.
       (Select smaller larger, Amend smaller larger)
    => Lens' (Many larger) (Many smaller)
project = lens select amend

-- | Polymorphic version of project'
project'
    :: forall smaller smaller' larger.
       (Select smaller larger, Amend' smaller smaller' larger)
    => Lens (Many larger) (Many (Replaces smaller smaller' larger)) (Many smaller) (Many smaller')
project' = lens select (amend' @smaller @smaller' Proxy)

-- | 'selectL' ('view' 'projectL') and 'amendL' ('set' 'projectL') in 'Lens'' form.
--
-- @
-- let x = False './' Tagged \@\"Hi" (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'O' './' 'nil'
-- x '^.' ('projectL' \@'[Foo, Bar] Proxy) \`shouldBe` Tagged \@Foo False './' Tagged \@Bar \'X' './' nil
-- (x '&' ('projectL' \@'[\"Hi", \"Bye"] Proxy) '.~' (Tagged \@\"Hi" (6 :: Int) './' Tagged \@\"Bye" \'P' './' nil)) '`shouldBe`
--     False './' Tagged \@\"Hi" (6 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'P' './' 'nil'
-- @
projectL
    :: forall ls smaller larger proxy.
       ( Select smaller larger
       , Amend smaller larger
       , smaller ~ KindsAtLabels ls larger
       , IsDistinct ls
       , UniqueLabels ls larger)
    => proxy ls -> Lens' (Many larger) (Many smaller)
projectL p = lens (selectL p) (amendL p)

-- | Polymorphic version of projectL'
--
-- @
-- let x = False './' Tagged \@\"Hi" (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'O' './' 'nil'
-- (x '&' ('projectL'' \@'[\"Hi", \"Bye"] Proxy) '.~' (True './' Tagged \@\"Changed" False './' 'nil')) \`shouldBe`
--     False './' True './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Changed" False './' 'nil'
-- @
projectL'
    :: forall ls smaller smaller' larger proxy.
       ( Select smaller larger
       , Amend' smaller smaller' larger
       , smaller ~ KindsAtLabels ls larger
       , IsDistinct ls
       , UniqueLabels ls larger)
    => proxy ls -> Lens (Many larger) (Many (Replaces smaller smaller' larger)) (Many smaller) (Many smaller')
projectL' p = lens (selectL p) (amendL' p)

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
projectN
    :: forall ns smaller larger proxy.
       (SelectN ns smaller larger, AmendN ns smaller larger)
    => proxy ns -> Lens' (Many larger) (Many smaller)
projectN p = lens (selectN p) (amendN p)

-- | Polymorphic version of 'projectN'
projectN'
    :: forall ns smaller smaller' larger proxy.
       (SelectN ns smaller larger, AmendN' ns smaller smaller' larger)
    => proxy ns -> Lens (Many larger) (Many (ReplacesIndex ns smaller' larger)) (Many smaller) (Many smaller')
projectN' p = lens (selectN p) (amendN' p)
