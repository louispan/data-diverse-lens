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
import Data.Proxy

-- | Like 'Strong' or 'Arrow' but lifting into 'Many'
itemized
    :: forall a' w a b. ( Profunctor w
       , Strong w
       , UniqueMember a a'
       , UniqueMember b (Replace a b a')
       )
    => w a b -> w (Many a') (Many (Replace a b a'))
itemized w = dimap (\c -> (fetch c, c)) (\(b, c) -> replace' (Proxy @a) c b) (first' w)

-- | Like 'Strong' or 'Arrow' but lifting into 'Many' of one type
itemized' :: Profunctor w => w a b -> w (Many '[a]) (Many '[b])
itemized' w = dimap fetch single w

-- | Like 'Strong' or 'Arrow' but lifting from a 'Many' to a 'Many' of another type
projected
    :: forall a' proxy w a b. ( Profunctor w
       , Strong w
       , Select a a'
       , Amend' a b a'
       )
    => proxy a' -> w (Many a) (Many b) -> w (Many a') (Many (Replaces a b a'))
projected _ w = dimap (\c -> (select c, c)) (\(b, c) -> amend' (Proxy @a) c b) (first' w)

-- | Split the input between the two argument arrows and combine their output.
-- The type of the resultant input is a 'Many' of all the unique types in the argument arrows' inputs,
-- The type of the resultant output is a concatenated 'Many' of the arguments arrows' outputs.
-- Analogous to a 'Many' combination of both of 'Control.Arrow.***' and 'Control.Arrow.&&&'.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs.
(*&&*)
    :: forall w a1 b1 a2 b2. ( C.Category w
       , Profunctor w
       , Strong w
       , Select a1 (AppendUnique a1 a2)
       , Select a2 (AppendUnique a1 a2)
       )
    => w (Many a1) (Many b1)
    -> w (Many a2) (Many b2)
    -> w (Many (AppendUnique a1 a2)) (Many (Append b1 b2))
x *&&* y = rmap (uncurry (/./)) (lmap (select @a1 &&& select @a2) (first' x) C.>>> second' y)
infixr 3 *&&* -- like ***

-- | Left-to-right chaining of arrows one after another,  where left over input not consumed
-- by the right arrow is forwarded to the output.
-- It is a compile error if the types are not distinct in each of the argument arrow inputs,
-- or if the input of the second arrow is not a subset of the output of the first arrow.
(>&&>)
    :: forall w a b1 a2 b2.
       ( C.Category w
       , Profunctor w
       , Strong w
       , Select (Complement b1 a2) b1
       , Select a2 b1
       )
    => w a (Many b1)
    -> w (Many a2) (Many b2)
    -> w a (Many (Append (Complement b1 a2) b2))
x >&&> y = rmap (uncurry (/./)) (rmap (select @(Complement b1 a2) &&& select @a2) x C.>>> (second' y))
infixr 3 >&&> -- like ***

-- | right-to-left version of '(>&&>)'
(<&&<) ::
       ( C.Category w
       , Profunctor w
       , Strong w
       , Select (Complement b1 a2) b1
       , Select a2 b1
       )
    => w (Many a2) (Many b2)
    -> w a (Many b1)
    -> w a (Many (Append (Complement b1 a2) b2))
(<&&<) = flip (>&&>)
infixl 2 <&&< -- like >&&>
