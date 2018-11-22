{-# language ExplicitForAll #-}
module Data.Variant1.Lens (module Data.Variant1, _Ctor1) where

import Control.Lens.Prism (Prism', prism')

import Data.Variant1

{-# inline _Ctor1 #-}
_Ctor1 :: forall g vs a b. Ctor1 vs g => Prism' (Variant1 vs a) (g a)
_Ctor1 = prism' inj1 prj1