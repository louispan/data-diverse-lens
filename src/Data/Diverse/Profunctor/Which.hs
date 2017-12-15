{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Diverse.Profunctor.Which (
      -- * Combinators similar to Profunctor Choice
      faceted
    , faceted'
    , injected
    , (+||+)
    , (>||>)
    , (<||<)
    ) where

import qualified Control.Category as C
import Control.Lens
import Data.Diverse.Which
import Data.Diverse.TypeLevel
import Data.Proxy

-- | Like 'Choice' or 'ArrowChoice' but lifting into 'Which'
faceted
    :: ( Profunctor w
       , Choice w
       , UniqueMember a a'
       , UniqueMember b b'
       , Diversify (Complement a' '[a]) b'
       )
    => w a b -> w (Which a') (Which b')
faceted w = dimap trial (either diversify pick) (right' w)

-- | Like 'Choice' or 'ArrowChoice' but lifting into 'Which' of one type
faceted' :: (Profunctor w, Choice w) => w a b -> w (Which '[a]) (Which '[b])
faceted' w = dimap trial (either impossible pick) (right' w)

-- | Like 'Choice' or 'ArrowChoice' but lifting from 'Which' into another type of 'Which'
-- NB. It is a compile error if all of the input types in the second arrow @a@
-- is not the output types of the first arrow.
-- This prevents surprising behaviour where the second arrow is ignored.
injected
    :: ( Profunctor w
       , Choice w
       , Reinterpret a a'
       , Diversify b (AppendUnique (Complement a' a) b)
       , Diversify (Complement a' a) (AppendUnique (Complement a' a) b)
       -- extra contraint to prevent surprises (see comment above)
       , Complement a a' ~ '[]
       )
    => proxy a'
    -> w (Which a) (Which b)
    -> w (Which a') (Which (AppendUnique (Complement a' a) b))
injected _ w = dimap reinterpret (either diversify diversify) (right' w)

-- | Split the input between the two argument arrows, retagging and merging their outputs.
-- The output is merged into a 'Which' of unique types.
-- Analogous to a 'Which' combination of both 'Control.Arrow.+++' and 'Control.Arrow.|||'.
-- It is a compile error if the types are not distinct after 'Append'ing both argument arrows inputs.
-- This is to prevent accidently processing an input type twice.
-- The compile error will be due to @(Append a1 a2)@ which will not satisfy
-- @UniqueMember@ constraints in 'Reinterpret'.
(+||+)
    :: forall w a1 b1 a2 b2.
       ( C.Category w
       , Profunctor w
       , Choice w
       , Reinterpret a2 (Append a1 a2)
       , a1 ~ Complement (Append a1 a2) a2
       , Diversify b1 (AppendUnique b1 b2)
       , Diversify b2 (AppendUnique b1 b2)
       )
    => w (Which a1) (Which b1)
    -> w (Which a2) (Which b2)
    -> w (Which (Append a1 a2)) (Which (AppendUnique b1 b2))
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
    :: forall w a b c d.
       ( C.Category w
       , Profunctor w
       , Choice w
       , Reinterpret c b
       , Diversify d (AppendUnique (Complement b c) d)
       , Diversify (Complement b c) (AppendUnique (Complement b c) d)
       , Complement c b ~ '[])
    => w a (Which b)
    -> w (Which c) (Which d)
    -> w a (Which (AppendUnique (Complement b c) d))
(>||>) hdl1 hdl2 = hdl1 C.>>> injected (Proxy @b) hdl2
infixr 2 >||> -- like +||+

-- | right-to-left version of '(>||>)'
(<||<)
    :: ( C.Category w
       , Profunctor w
       , Choice w
       , Reinterpret c b
       , Diversify d (AppendUnique (Complement b c) d)
       , Diversify (Complement b c) (AppendUnique (Complement b c) d)
       , Complement c b ~ '[])
    => w (Which c) (Which d)
    -> w a (Which b)
    -> w a (Which (AppendUnique (Complement b c) d))
(<||<) = flip (>||>)
infixl 2 <||< -- like >||>
