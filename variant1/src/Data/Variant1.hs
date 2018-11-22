{-# language KindSignatures, DataKinds, PolyKinds #-}
{-# language TypeOperators #-}
{-# language FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
{-# language UndecidableInstances #-}
{-# language ScopedTypeVariables #-}
{-# language InstanceSigs #-}
{-# language ConstraintKinds, TypeFamilies #-}
{-# language RoleAnnotations #-}
{-# language FunctionalDependencies #-}
module Data.Variant1
  ( Variant1
  , absurdV1
  , widenV1
  , elimV1
  , Ctor1(..)
  )
where

import Data.Variant1.Internal
