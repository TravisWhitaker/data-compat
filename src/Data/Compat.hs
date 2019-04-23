{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , KindSignatures
           , RankNTypes
           , ScopedTypeVariables
           , TypeFamilies
           , TypeApplications
           #-}

module Data.Compat where

import Control.Applicative

import Control.Monad.Trans.Reader

import Data.Constraint

import Data.Proxy

import Data.Void

import GHC.TypeLits

class Compat a where
    type Pred a             :: *
    type CompatConstraint a :: * -> Constraint
    type CompatF a          :: * -> *
    migrate  :: Pred a -> (CompatF a) a
    continue :: Proxy a
             -> Maybe (Dict ( Compat (Pred a)
                            , (CompatConstraint a) (Pred a)
                            , CompatConstraint a ~ CompatConstraint (Pred a)
                            , CompatF a ~ CompatF (Pred a)
                            )
                      )

getCompatible
    :: forall a.
       ( Compat a
       , (CompatConstraint a) a
       , Alternative (CompatF a)
       , Monad (CompatF a)
       )
    => (forall c. (Compat c, (CompatConstraint a) c) => (CompatF a) c)
    -> (CompatF a) a
getCompatible f =
    let f' = case continue (Proxy :: Proxy a) of
                Nothing   -> empty
                Just Dict -> getCompatible f >>= migrate
    in f <|> f'
