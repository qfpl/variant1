{-# language KindSignatures, DataKinds, PolyKinds #-}
{-# language TypeOperators #-}
{-# language FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
module Data.Variant1
  ( Variant1
  , absurdV1
  , widenV1
  , elimV1
  , Ctor1
  , inj1
  , prj1
  )
where

import Data.Variant1.Internal
