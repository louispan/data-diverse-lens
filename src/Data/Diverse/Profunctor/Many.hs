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
      Have
    , have
    , haveK
    , Projected
    , projected
    , projectedK
    , MakeFrom
    , MakeBoth
    , makeBesides
    , makeBesidesK
    , thenMake
    , thenMakeK
    ) where

import Control.Arrow
import qualified Control.Category as C
import Control.Lens
import Data.Diverse.Lens.Many
import Data.Diverse.Many
import Data.Diverse.TypeLevel
import Data.Profunctor

-- | A friendlier constraint synonym for 'have'.
type Have a b s t =
    ( Had a s
    , t ~ Replaced a b s
    )

-- | Like 'Strong' or 'Arrow' but lifting into 'Many'
have ::
    forall w a b s t.
    ( Profunctor w
    , Strong w
    , Have a b s t
    )
    => w a b -> w s t
have w = dimap (\c -> (view hasLens c, c)) (\(b, c) -> set (hadLens @a) b c) (first' w)

-- | 'have' under 'Kleisli'
haveK ::
    forall m a b s t.
    ( Monad m
    , Have a b s t
    )
    => (a -> m b) -> (s -> m t)
haveK f = runKleisli . have $ Kleisli f

-- | A friendlier constraint synonym for 'projected'.
type Projected a1 a2 b1 b2 =
    ( Select a1 a2
    , Amend a1 b1 a2
    , b2 ~ Replaces a1 b1 a2
    )

-- | Like 'Strong' or 'Arrow' but lifting from a 'Many' to a 'Many' of another type
projected :: forall w a1 a2 b1 b2.
    ( Strong w
    , Projected a1 a2 b1 b2
    )
    => w (Many a1) (Many b1) -> w (Many a2) (Many b2)
projected w = dimap (\c -> (select c, c)) (\(b, c) -> amend @a1 @b1 @a2 c b) (first' w)

-- | 'projected' under 'Kleisli'
projectedK :: forall m a1 a2 b1 b2.
    ( Monad m
    , Projected a1 a2 b1 b2
    )
    => (Many a1 -> m (Many b1)) -> (Many a2 -> m (Many b2))
projectedK f = runKleisli . projected $ Kleisli f

-- | A friendlier constraint synonym for '*&&*'.
type MakeBoth b1 b2 b3 =
    ( b3 ~ Append b1 b2
    )

type MakeFrom a1 a2 a3 =
    ( Select a1 a3
    , Select a2 a3
    , a3 ~ AppendUnique a1 a2
    )

-- | Split the input between the two argument arrows and combine their output.
-- The type of the resultant input is a 'Many' of all the unique types in the argument arrows' inputs,
-- The type of the resultant output is a concatenated 'Many' of the arguments arrows' outputs.
-- Analogous to a 'Many' combination of both of 'Control.Arrow.***' and 'Control.Arrow.&&&'.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs.
makeBesides
    :: forall w a1 a2 a3 b1 b2 b3.
    ( C.Category w
    , Strong w
    , MakeFrom a1 a2 a3
    , MakeBoth b1 b2 b3
    )
    => w (Many a1) (Many b1)
    -> w (Many a2) (Many b2)
    -> w (Many a3) (Many b3)
x `makeBesides` y = rmap (uncurry (/./)) (lmap (select @a1 &&& select @a2) (first' x) C.>>> second' y)
infixr 3 `makeBesides` -- like ***

makeBesidesK
    :: forall m a1 a2 a3 b1 b2 b3.
    ( Monad m
    , MakeFrom a1 a2 a3
    , MakeBoth b1 b2 b3
    )
    => (Many a1 -> m (Many b1))
    -> (Many a2 -> m (Many b2))
    -> (Many a3 -> m (Many b3))
makeBesidesK f g = runKleisli $ makeBesides (Kleisli f) (Kleisli g)
infixr 3 `makeBesidesK` -- like ***

-- | Left-to-right chaining of arrows one after another, where left over input not consumed
-- by the right arrow is forwarded to the output.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs,
-- or if the input of the second arrow is not a complete subset of the output of the first arrow.
thenMake :: forall w a a2 b1 b2 b3.
    ( C.Category w
    , Strong w
    , Projected a2 b1 b2 b3
    )
    => w a (Many b1)
    -> w (Many a2) (Many b2)
    -> w a (Many b3)
x `thenMake` y = x C.>>> projected y
infixr 3 `thenMake` -- like ***

thenMakeK :: forall m a a2 b1 b2 b3.
    ( Monad m
    , Projected a2 b1 b2 b3
    )
    => (a -> m (Many b1))
    -> (Many a2 -> m (Many b2))
    -> (a -> m (Many b3))
thenMakeK f g = runKleisli $ thenMake (Kleisli f) (Kleisli g)
infixr 3 `thenMakeK` -- like ***

-----

-- type Besides a1 a2 a3 = (a3 ~ Append a1 a2)

-- besides :: (Applicative f, Besides a1 a2 a3)
--     => f (Many a1) -> f (Many a2) -> f (Many a3)
-- besides x y = liftA2 (/./) x y
-- infixr 5 `besides` -- like (/./) and (++)

-- besides2 :: (Biapplicative f, Besides a1 a2 a3, Besides b1 b2 b3)
--     => f (Many a1) (Many b1) -> f (Many a2) (Many b2) -> f (Many a3) (Many b3)
-- besides2 x y = biliftA2 (/./) (/./) x y
-- infixr 5 `besides2` -- like (/./) and (++)
