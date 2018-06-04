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
    , facetedK
    , Injected
    , injected
    , injectedK
    , ChooseFrom
    , ChooseBoth
    -- , ChooseBetween
    , chooseBetween
    , chooseBetweenK
    , thenChoose
    , thenChooseK
    , chooseWith
    -- , (+||+)
    -- , (>||>)
    -- , (<||<)
    ) where

import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import Data.Diverse.Lens
import Data.Semigroup

-- | A friendlier constraint synonym for 'faceted'.
type Faceted a as x b bs y =
    ( MatchingFacet a x y
    , AsFacet b y
    )

-- | Like 'Choice' or 'ArrowChoice' but lifting into a polymorphic variant.
faceted :: forall w a as x b bs y.
    ( Choice w
    , Faceted a as x b bs y
    )
    => w a b -> w x y
faceted w = dimap (matchingFacet @a @x @y)
                   (either id (review facet))
                   (right' w)

-- | 'faceted' under 'Kleisli'
facetedK :: forall m a as x b bs y.
    ( Monad m
    , Faceted a as x b bs y
    )
    => (a -> m b) -> (x -> m y)
facetedK f = runKleisli . faceted $ Kleisli f

-- | A friendlier constraint synonym for 'injected'.
type Injected a2 a3 b2 b3 =
    ( Reinterpret a2 a3
    , ChooseBoth (Complement a3 a2) b2 b3
    -- extra contraint to prevent surprises (see comment for 'injected')
    , Complement a2 a3 ~ '[]
    )

-- | Like 'Choice' or 'ArrowChoice' but lifting from 'Which' into another type of 'Which'
-- NB. It is a compile error if all of the input types in the second arrow @a@
-- is not the output types of the first arrow.
-- This prevents surprising behaviour where the second arrow is completely ignored.
injected :: forall w a2 a3 b2 b3.
    ( Choice w
    , Injected a2 a3 b2 b3
    )
    => w (Which a2) (Which b2)
    -> w (Which a3) (Which b3)
injected w = dimap (reinterpret @a2 @a3) (either diversify diversify) (right' w)

-- | 'injected' under 'Kleisli'
injectedK :: forall m a2 a3 b2 b3.
    ( Monad m
    , Injected a2 a3 b2 b3
    )
    => (Which a2 -> m (Which b2))
    -> (Which a3 -> m (Which b3))
injectedK f = runKleisli . injected $ Kleisli f

-- | A friendlier constraint synonym for 'chooseBoth'.
-- Redundant constraint: @b3 ~ AppendUnique b1 b2@ is redundant but narrows down @b3@
type ChooseBoth b1 b2 b3 =
    ( Diversify b1 b3
    , Diversify b2 b3
    , b3 ~ AppendUnique b1 b2
    )

-- chooseBoth ::
--     ( C.Category w
--     , Strong w
--     , ChooseBoth b1 b2 b3
--     )
--     => w a (Which b1)
--     -> w a (Which b2)
--     -> w a (Which b3, Which b3)
-- chooseBoth x y = lmap (\a -> (a, a)) (first' (rmap diversify x)) C.>>> (second' (rmap diversify y))
-- infixr 2 `chooseBoth` -- like +++

-- | A friendlier constraint synonym for 'chooseFrom'.
-- Redundant constraint: @a3 ~ Append a1 a2@ is redundant but narrows down @a3@
type ChooseFrom a1 a2 a3 =
    ( Reinterpret a2 a3
    , a1 ~ Complement a3 a2
    , a3 ~ Append a1 a2
    )

-- -- | A friendlier constraint synonym for 'chooseBetween'.
-- type ChooseBetween a1 a2 a3 b1 b2 b3 =
--     ( ChooseFrom a1 a2 a3
--     , ChooseBoth b1 b2 b3
--     )

-- | Split the input between the two argument arrows, retagging and merging their outputs.
-- The output is merged into a 'Which' of unique types.
-- Analogous to a 'Which' combination of both 'Control.Arrow.+++' and 'Control.Arrow.|||'.
-- It is a compile error if the types are not distinct after 'Append'ing both argument arrows inputs.
-- This is to prevent accidently processing an input type twice.
-- The compile error will be due to @(Append a1 a2)@ which will not satisfy
-- @UniqueMember@ constraints in 'Reinterpret'.
chooseBetween :: forall w a1 a2 a3 b1 b2 b3.
    ( C.Category w
    , Choice w
    , ChooseFrom a1 a2 a3
    , ChooseBoth b1 b2 b3
    )
    => w (Which a1) (Which b1)
    -> w (Which a2) (Which b2)
    -> w (Which a3) (Which b3)
x `chooseBetween` y =
    rmap
        (either diversify diversify)
        (lmap (reinterpret @a2 @a3) (left' x) C.>>> right' y)
infixr 2 `chooseBetween` -- like +++

-- | 'chooseBetween' under 'Kleisli'
chooseBetweenK :: forall m a1 a2 a3 b1 b2 b3.
    (Monad m, ChooseFrom a1 a2 a3, ChooseBoth b1 b2 b3)
    => (Which a1 -> m (Which b1))
    -> (Which a2 -> m (Which b2))
    -> (Which a3 -> m (Which b3))
chooseBetweenK f g = runKleisli $ chooseBetween (Kleisli f) (Kleisli g)
infixr 2 `chooseBetweenK` -- like +++

-- (+||+) ::
--     ( C.Category w
--     , Choice w
--     , ChooseBetween a1 a2 a3 b1 b2 b3
--     )
--     => w (Which a1) (Which b1)
--     -> w (Which a2) (Which b2)
--     -> w (Which a3) (Which b3)
-- (+||+) = chooseBetween
-- infixr 2 +||+ -- like +++

-- | Left-to-right chaining of arrows one after another, where left over possibilities not handled
-- by the right arrow is forwarded to the output.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs,
-- or if the types are not distinct in each of the argument arrow output.
-- NB. It is currently not a compile error if the input of the second arrow is distinct from the
-- output of the first arrrow, in which case this function does not change anything
-- except to add the types of the second arrow to the output.
thenChoose :: forall w a a2 b1 b2 b3.
       ( C.Category w
       , Choice w
       , Injected a2 b1 b2 b3
       )
    => w a (Which b1)
    -> w (Which a2) (Which b2)
    -> w a (Which b3)
hdl1 `thenChoose` hdl2 = hdl1 C.>>> injected hdl2
infixr 2 `thenChoose` -- like +++

-- | 'thenChoose' under 'Kleisli'
thenChooseK :: forall m a a2 b1 b2 b3.
       (Monad m, Injected a2 b1 b2 b3)
    => (a -> m (Which b1))
    -> (Which a2 -> m (Which b2))
    -> (a -> m (Which b3))
thenChooseK f g = runKleisli $ thenChoose (Kleisli f) (Kleisli g)
infixr 2 `thenChooseK` -- like +++

-- -- | right-to-left version of '(>||>)'
-- (<||<)
--     :: forall w a a2 b1 b2 b3.
--        ( C.Category w
--        , Choice w
--        , Injected a2 b1 b2 b3
--        )
--     => w (Which a2) (Which b2)
--     -> w a (Which b1)
--     -> w a (Which b3)
-- (<||<) = flip (>||>)
-- infixr 2 <||< -- like >||>

------------------------------------------

chooseWith :: (Semigroup (f (Which a3)), Functor f, ChooseBoth a1 a2 a3) => (f (Which a3) -> f (Which a3) -> f (Which a3)) -> f (Which a1) -> f (Which a2) -> f (Which a3)
chooseWith f x y = (diversify <$> x) `f` (diversify <$> y)
infixr 6 `chooseWith` -- like mappend

