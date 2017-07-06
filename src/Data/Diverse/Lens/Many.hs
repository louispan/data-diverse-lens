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
import Data.Proxy
import Data.Tagged
import Data.Diverse.Many
import Data.Diverse.TypeLevel

-- | @_Many = iso fromMany toMany@
_Many :: IsMany t xs a => Iso' (Many xs) (t xs a)
_Many = iso fromMany toMany
{-# INLINE _Many #-}

-- | @_Many' = iso fromMany' toMany'@
_Many' :: IsMany Tagged xs a => Iso' (Many xs) a
_Many' = iso fromMany' toMany'
{-# INLINE _Many' #-}

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
{-# INLINE item #-}

-- | Polymorphic version of 'item'
item' :: forall x y xs. UniqueMember x xs => Lens (Many xs) (Many (Replace x y xs)) x y
item' = lens fetch (replace' @x @y Proxy)
{-# INLINE item' #-}


-- | 'fetchL' ('view' 'itemL') and 'replaceL' ('set' 'itemL') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' 'nil'
-- x '^.' 'itemL' \@Foo Proxy \`shouldBe` Tagged \@Foo False
-- (x '&' 'itemL' \@Foo Proxy '.~' Tagged \@Foo True) \`shouldBe` (5 :: Int) './' Tagged \@Foo True './' Tagged \@Bar \'X' './' 'nil'
-- @
itemL :: forall l xs x proxy. (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => proxy l -> Lens' (Many xs) x
itemL p = lens (fetchL p) (replaceL p)
{-# INLINE itemL #-}

-- | Polymorphic version of 'itemL'
--
-- @
-- let x = (5 :: Int) './' Tagged @Foo False './' Tagged \@Bar \'X' './' 'nil'
-- (x '&' itemL' \@Foo Proxy '.~' \"foo") \`shouldBe` (5 :: Int) './' \"foo" './' Tagged \@Bar \'X' './' 'nil'
-- @
itemL' :: forall l y xs x proxy. (UniqueLabelMember l xs, x ~ KindAtLabel l xs) => proxy l -> Lens (Many xs) (Many (Replace x y xs)) x y
itemL' p = lens (fetchL p) (replaceL' p)
{-# INLINE itemL' #-}


-- | 'fetchN' ('view' 'item') and 'replaceN' ('set' 'item') in 'Lens'' form.
--
-- @
-- let x = (5 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' ./ nil
-- x '^.' 'itemN' (Proxy \@0) \`shouldBe` 5
-- (x '&' 'itemN' (Proxy @0) '.~' 6) \`shouldBe` (6 :: Int) './' False './' \'X' './' Just \'O' './' (6 :: Int) './' Just \'A' './' 'nil'
-- @
itemN ::  forall n x xs proxy. MemberAt n x xs => proxy n -> Lens' (Many xs) x
itemN p = lens (fetchN p) (replaceN p)
{-# INLINE itemN #-}


-- | Polymorphic version of 'itemN'
itemN' ::  forall n x y xs proxy. MemberAt n x xs => proxy n -> Lens (Many xs) (Many (ReplaceIndex n y xs)) x y
itemN' p = lens (fetchN p) (replaceN' @n @x @y p)
{-# INLINE itemN' #-}

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
{-# INLINE project #-}

-- | Polymorphic version of project'
project'
    :: forall smaller smaller' larger zipped.
       (Select smaller larger, Amend' smaller smaller' larger zipped)
    => Lens (Many larger) (Many (Replaces smaller smaller' larger)) (Many smaller) (Many smaller')
project' = lens select (amend' @smaller @smaller' Proxy)
{-# INLINE project' #-}

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
{-# INLINE projectL #-}

-- | Polymorphic version of projectL'
--
-- @
-- let x = False './' Tagged \@\"Hi" (5 :: Int) './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Bye" \'O' './' 'nil'
-- (x '&' ('projectL'' \@'[\"Hi", \"Bye"] Proxy) '.~' (True './' Tagged \@\"Changed" False './' 'nil')) \`shouldBe`
--     False './' True './' Tagged \@Foo False './' Tagged \@Bar \'X' './' Tagged \@\"Changed" False './' 'nil'
-- @
projectL'
    :: forall ls smaller smaller' larger proxy zipped.
       ( Select smaller larger
       , Amend' smaller smaller' larger zipped
       , smaller ~ KindsAtLabels ls larger
       , IsDistinct ls
       , UniqueLabels ls larger)
    => proxy ls -> Lens (Many larger) (Many (Replaces smaller smaller' larger)) (Many smaller) (Many smaller')
projectL' p = lens (selectL p) (amendL' p)
{-# INLINE projectL' #-}

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
{-# INLINE projectN #-}

-- | Polymorphic version of 'projectN'
projectN'
    :: forall ns smaller smaller' larger proxy zipped.
       (SelectN ns smaller larger, AmendN' ns smaller smaller' larger zipped)
    => proxy ns -> Lens (Many larger) (Many (ReplacesIndex ns smaller' larger)) (Many smaller) (Many smaller')
projectN' p = lens (selectN p) (amendN' p)
{-# INLINE projectN' #-}
