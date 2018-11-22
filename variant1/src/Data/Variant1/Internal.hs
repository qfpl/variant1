{-# language DataKinds, KindSignatures, PolyKinds #-}
{-# language FlexibleInstances, FlexibleContexts #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language InstanceSigs #-}
{-# language RoleAnnotations #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}
module Data.Variant1.Internal where

import Data.Functor.Classes (Show1(..), showsPrec1)
import Data.Generics.Fixplate.Base (ShowF(..))
import Data.Kind (Constraint)
import GHC.Base (Any)
import GHC.TypeLits (TypeError, ErrorMessage(..))
import Unsafe.Coerce (unsafeCoerce)

data Variant1 (vs :: [k -> *]) (a :: k)
  = Variant1 {-# unpack #-} !Word Any
type role Variant1 representational representational

{-# inline elimV1 #-}
elimV1
  :: forall a as x r
   . (a x -> r)
  -> (Variant1 as x -> r)
  -> Variant1 (a ': as) x -> r
elimV1 f g (Variant1 tag a) =
  if tag == 0
  then f (unsafeCoerce a :: a x)
  else g (Variant1 (tag-1) a)

{-# inline widenV1 #-}
widenV1 :: Variant1 as x -> Variant1 (a ': as) x
widenV1 (Variant1 tag a) = Variant1 (tag+1) a

{-# inline absurdV1 #-}
absurdV1 :: Variant1 '[] a -> b
absurdV1 _ = error "absurdV1: absurd!"

instance Show1 (Variant1 vs) => ShowF (Variant1 vs) where
  {-# inline showsPrecF #-}
  showsPrecF = showsPrec

instance Functor (Variant1 '[]) where
  {-# inline fmap #-}
  fmap _ = absurdV1

instance Foldable (Variant1 '[]) where
  {-# inline foldMap #-}
  foldMap _ = absurdV1

instance Traversable (Variant1 '[]) where
  {-# inline traverse #-}
  traverse _ = absurdV1

instance (Functor v, Functor (Variant1 vs)) => Functor (Variant1 (v ': vs)) where
  {-# inline fmap #-}
  fmap f =
    elimV1
      (inj1 . fmap f)
      (widenV1 . fmap f)

instance (Foldable v, Foldable (Variant1 vs)) => Foldable (Variant1 (v ': vs)) where
  {-# inline foldMap #-}
  foldMap f =
    elimV1
      (foldMap f)
      (foldMap f)

instance (Traversable v, Traversable (Variant1 vs)) => Traversable (Variant1 (v ': vs)) where
  {-# inline traverse #-}
  traverse f =
    elimV1
      (fmap inj1 . traverse f)
      (fmap widenV1 . traverse f)

instance Show1 (Variant1 '[]) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec _ _ _ = absurdV1

instance (Show1 v, Show1 (Variant1 vs)) => Show1 (Variant1 (v ': vs)) where
  {-# inline liftShowsPrec #-}
  liftShowsPrec a b c =
    elimV1
      (\val ->
         showParen (c > 10) $
         showString "Variant1 " .
         liftShowsPrec a b 11 val)
      (liftShowsPrec a b c)

instance (Show1 (Variant1 vs), Show a) => Show (Variant1 vs a) where
  {-# inline showsPrec #-}
  showsPrec = showsPrec1

class Ctor1 as g | as -> g where
  inj1 :: g a -> Variant1 as a
  prj1 :: Variant1 as a -> Maybe (g a)

instance {-# overlapping #-} Ctor1 (v ': vs) v where
  {-# inline inj1 #-}
  inj1 = Variant1 0 . unsafeCoerce

  {-# inline prj1 #-}
  prj1 = elimV1 Just $ const Nothing

instance {-# overlappable #-} Ctor1 vs b =>  Ctor1 (v ': vs) b where
  {-# inline inj1 #-}
  inj1 = widenV1 . inj1

  {-# inline prj1 #-}
  prj1 :: forall as a g. Ctor1 vs g => Variant1 (v ': vs) a -> Maybe (g a)
  prj1 (Variant1 tag a) = prj1 (Variant1 (tag-1) a :: Variant1 vs a)
