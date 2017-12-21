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
      itemized
    , itemized'
    , projected
    , (*&&*)
    , (>&&>)
    , (<&&<)
    ) where

import qualified Control.Category as C
import Control.Arrow
import Control.Lens
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import Data.Profunctor

-- | A friendlier constraint synonym for 'itemized'.
type Itemized w a as b bs =
    ( Profunctor w
    , Strong w
    , UniqueMember a as
    , UniqueMember b bs
    , bs ~ Replace a b as
    )

-- | Like 'Strong' or 'Arrow' but lifting into 'Many'
itemized
    :: forall w a as b bs. (Itemized w a as b bs)
    => w a b -> w (Many as) (Many bs)
itemized w = dimap (\c -> (fetch c, c)) (\(b, c) -> replace @a c b) (first' w)

-- | Like 'Strong' or 'Arrow' but lifting into 'Many' of one type
itemized' :: Profunctor w => w a b -> w (Many '[a]) (Many '[b])
itemized' w = dimap fetch single w

-- | A friendlier constraint synonym for 'projected'.
type Projected w a1 a2 b1 b2 =
    ( Profunctor w
    , Strong w
    , Select a1 a2
    , Amend a1 b1 a2
    , b2 ~ Replaces a1 b1 a2
    )

-- | Like 'Strong' or 'Arrow' but lifting from a 'Many' to a 'Many' of another type
projected
    :: forall proxy w a1 a2 b1 b2. (Projected w a1 a2 b1 b2)
    => proxy a2 -> w (Many a1) (Many b1) -> w (Many a2) (Many b2)
projected _ w = dimap (\c -> (select c, c)) (\(b, c) -> amend @a1 c b) (first' w)

-- | A friendlier constraint synonym for '*&&*'.
type SelectWith w a1 a2 a3 b1 b2 b3 =
    ( C.Category w
    , Profunctor w
    , Strong w
    , Select a1 (AppendUnique a1 a2)
    , Select a2 (AppendUnique a1 a2)
    , a3 ~ AppendUnique a1 a2
    , b3 ~ Append b1 b2
    )

-- | Split the input between the two argument arrows and combine their output.
-- The type of the resultant input is a 'Many' of all the unique types in the argument arrows' inputs,
-- The type of the resultant output is a concatenated 'Many' of the arguments arrows' outputs.
-- Analogous to a 'Many' combination of both of 'Control.Arrow.***' and 'Control.Arrow.&&&'.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs.
(*&&*)
    :: forall w a1 a2 a3 b1 b2 b3. (SelectWith w a1 a2 a3 b1 b2 b3)
    => w (Many a1) (Many b1)
    -> w (Many a2) (Many b2)
    -> w (Many a3) (Many b3)
x *&&* y = rmap (uncurry (/./)) (lmap (select @a1 &&& select @a2) (first' x) C.>>> second' y)
infixr 3 *&&* -- like ***

-- | A friendlier constraint synonym for '>&&>'.
type ThenSelect w a2 b1 b2 b3 =
    ( C.Category w
    , Profunctor w
    , Strong w
    , Select (Complement b1 a2) b1
    , Select a2 b1
    , b3 ~ Append (Complement b1 a2) b2
    )

-- | Left-to-right chaining of arrows one after another, where left over input not consumed
-- by the right arrow is forwarded to the output.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs,
-- or if the input of the second arrow is not a subset of the output of the first arrow.
(>&&>)
    :: forall w a a2 b1 b2 b3.
       (ThenSelect w a2 b1 b2 b3)
    => w a (Many b1)
    -> w (Many a2) (Many b2)
    -> w a (Many b3)
x >&&> y = rmap (uncurry (/./)) (rmap (select @(Complement b1 a2) &&& select @a2) x C.>>> (second' y))
infixr 3 >&&> -- like ***

-- | right-to-left version of '(>&&>)'
(<&&<) ::
       (ThenSelect w a2 b1 b2 b3)
    => w (Many a2) (Many b2)
    -> w a (Many b1)
    -> w a (Many b3)
(<&&<) = flip (>&&>)
infixl 2 <&&< -- like >&&>
