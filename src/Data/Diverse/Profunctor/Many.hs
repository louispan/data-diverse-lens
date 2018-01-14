{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Diverse.Profunctor.Many (
      -- * Combinators similar to Profunctor Strong
      Itemized
    , itemized
    , itemized'
    , Projected
    , projected
    , ProduceBoth
    , (*&&*)
    , ThenProduce
    , (>&&>)
    , (<&&<)
    ) where

import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import Data.Diverse.Lens.Many
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import Data.Profunctor

-- | A friendlier constraint synonym for 'itemized'.
type Itemized a b s t =
    ( HasItem a b s t
    , HasItem' a s
    )

-- | Like 'Strong' or 'Arrow' but lifting into 'Many'
itemized ::
    forall w a b s t.
    ( Profunctor w
    , Strong w
    , Itemized a b s t
    )
    => w a b -> w s t
itemized w = dimap (\c -> (view item' c, c)) (\(b, c) -> set (item @a) b c) (first' w)

-- | Like 'Strong' or 'Arrow' but lifting into 'Many' of one type
itemized' :: Profunctor w => w a b -> w (Many '[a]) (Many '[b])
itemized' = dimap fetch single

-- | A friendlier constraint synonym for 'projected'.
type Projected a1 a2 b1 b2 =
    ( Select a1 a2
    , Amend a1 b1 a2
    , b2 ~ Replaces a1 b1 a2
    )

-- | Like 'Strong' or 'Arrow' but lifting from a 'Many' to a 'Many' of another type
projected :: forall proxy w a1 a2 b1 b2.
    ( Strong w
    , Projected a1 a2 b1 b2
    )
    => proxy a2 -> w (Many a1) (Many b1) -> w (Many a2) (Many b2)
projected _ w = dimap (\c -> (select c, c)) (\(b, c) -> amend @a1 c b) (first' w)

-- | A friendlier constraint synonym for '*&&*'.
type ProduceBoth a1 a2 a3 b1 b2 b3 =
    ( Select a1 a3
    , Select a2 a3
    , a3 ~ AppendUnique a1 a2
    , b3 ~ Append b1 b2
    )

-- | Split the input between the two argument arrows and combine their output.
-- The type of the resultant input is a 'Many' of all the unique types in the argument arrows' inputs,
-- The type of the resultant output is a concatenated 'Many' of the arguments arrows' outputs.
-- Analogous to a 'Many' combination of both of 'Control.Arrow.***' and 'Control.Arrow.&&&'.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs.
(*&&*)
    :: forall w a1 a2 a3 b1 b2 b3.
    ( C.Category w
    , Strong w
    , ProduceBoth a1 a2 a3 b1 b2 b3
    )
    => w (Many a1) (Many b1)
    -> w (Many a2) (Many b2)
    -> w (Many a3) (Many b3)
x *&&* y = rmap (uncurry (/./)) (lmap (select @a1 &&& select @a2) (first' x) C.>>> second' y)
infixr 3 *&&* -- like ***

-- | A friendlier constraint synonym for '>&&>'.
type ThenProduce a2 b1 b2 b3 =
    ( Select (Complement b1 a2) b1
    , Select a2 b1
    , b3 ~ Append (Complement b1 a2) b2
    )

-- | Left-to-right chaining of arrows one after another, where left over input not consumed
-- by the right arrow is forwarded to the output.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs,
-- or if the input of the second arrow is not a complete subset of the output of the first arrow.
(>&&>)
    :: forall w a a2 b1 b2 b3.
    ( C.Category w
    , Strong w
    , ThenProduce a2 b1 b2 b3
    )
    => w a (Many b1)
    -> w (Many a2) (Many b2)
    -> w a (Many b3)
x >&&> y = rmap (uncurry (/./)) (rmap (select @(Complement b1 a2) &&& select @a2) x C.>>> second' y)
infixr 3 >&&> -- like ***

-- | right-to-left version of '(>&&>)'
(<&&<) ::
    ( C.Category w
    , Strong w
    , ThenProduce a2 b1 b2 b3
    )
    => w (Many a2) (Many b2)
    -> w a (Many b1)
    -> w a (Many b3)
(<&&<) = flip (>&&>)
infixl 2 <&&< -- like >&&>
