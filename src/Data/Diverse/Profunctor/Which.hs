{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Diverse.Profunctor.Which (
      -- * Combinators similar to Profunctor Choice
      Faceted
    , faceted
    , faceted'
    , Injected
    , injected
    , ChooseBetween
    , (+||+)
    , (>||>)
    , (<||<)
    ) where

import qualified Control.Category as C
import Control.Lens
import Data.Diverse.Which
import Data.Diverse.TypeLevel
import Data.Diverse.Lens

-- | A friendlier constraint synonym for 'faceted'.
type Faceted a as x b bs y =
    ( MatchingFacet a x y
    , AsFacet b y
    )

-- | Like 'Choice' or 'ArrowChoice' but lifting into a polymorphic variant.
faceted :: forall w a as x b bs y.
    (Profunctor w
    , Choice w
    , Faceted a as x b bs y
    )
    => w a b -> w x y
faceted w = dimap (matchingFacet @a @x @y)
                   (either id (review facet))
                   (right' w)

-- | Like 'Choice' or 'ArrowChoice' but lifting into 'Which' of one type
faceted' :: (Profunctor w, Choice w) => w a b -> w (Which '[a]) (Which '[b])
faceted' w = dimap obvious pickOnly w

-- | A friendlier constraint synonym for 'injected'.
type Injected a a' b b' =
    ( Reinterpret a a'
    , Diversify b (AppendUnique (Complement a' a) b)
    , Diversify (Complement a' a) (AppendUnique (Complement a' a) b)
    , b' ~ AppendUnique (Complement a' a) b
    -- extra contraint to prevent surprises (see comment for 'injected')
    , Complement a a' ~ '[]
    )

-- | Like 'Choice' or 'ArrowChoice' but lifting from 'Which' into another type of 'Which'
-- NB. It is a compile error if all of the input types in the second arrow @a@
-- is not the output types of the first arrow.
-- This prevents surprising behaviour where the second arrow is ignored.
injected ::
    ( Profunctor w
    , Choice w
    , Injected a a' b b'
    )
    => w (Which a) (Which b)
    -> w (Which a') (Which b')
injected w = dimap reinterpret (either diversify diversify) (right' w)

-- | A friendlier constraint synonym for '+||+'.
type ChooseBetween a1 a2 a3 b1 b2 b3 =
    ( Reinterpret a2 (Append a1 a2)
    , a1 ~ Complement (Append a1 a2) a2
    , a3 ~ Append a1 a2
    , Diversify b1 (AppendUnique b1 b2)
    , Diversify b2 (AppendUnique b1 b2)
    , b3 ~ AppendUnique b1 b2
    )

-- | Split the input between the two argument arrows, retagging and merging their outputs.
-- The output is merged into a 'Which' of unique types.
-- Analogous to a 'Which' combination of both 'Control.Arrow.+++' and 'Control.Arrow.|||'.
-- It is a compile error if the types are not distinct after 'Append'ing both argument arrows inputs.
-- This is to prevent accidently processing an input type twice.
-- The compile error will be due to @(Append a1 a2)@ which will not satisfy
-- @UniqueMember@ constraints in 'Reinterpret'.
(+||+)
    :: forall w a1 a2 a3 b1 b2 b3.
       ( C.Category w
       , Profunctor w
       , Choice w
       , ChooseBetween a1 a2 a3 b1 b2 b3
       )
    => w (Which a1) (Which b1)
    -> w (Which a2) (Which b2)
    -> w (Which a3) (Which b3)
x +||+ y =
    rmap
        (either diversify diversify)
        (lmap (reinterpret @a2 @(Append a1 a2)) (left' x) C.>>> right' y)
infixr 2 +||+ -- like +++

-- | Left-to-right chaining of arrows one after another,  where left over possibilities not handled
-- by the right arrow is forwarded to the output.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs,
-- or if the types are not distinct of each of the argument arrow output,
-- or if the input of the second arrow is not a subset of the output of the first arrow.
-- This is to prevent surprises behaviour of the second arrow being ignored.
-- The compile error will be due to the @Complement c b ~ '[]@ constraint.
(>||>)
    :: forall w a a2 b1 b2 b3.
       ( C.Category w
       , Choice w
       , Injected a2 b1 b2 b3
       )
    => w a (Which b1)
    -> w (Which a2) (Which b2)
    -> w a (Which b3)
(>||>) hdl1 hdl2 = hdl1 C.>>> injected @_ @_ @b1 hdl2
infixr 2 >||> -- like +||+

-- | right-to-left version of '(>||>)'
(<||<)
    :: forall w a a2 b1 b2 b3.
       ( C.Category w
       , Choice w
       , Injected a2 b1 b2 b3
       )
    => w (Which a2) (Which b2)
    -> w a (Which b1)
    -> w a (Which b3)
(<||<) = flip (>||>)
infixl 2 <||< -- like >||>
