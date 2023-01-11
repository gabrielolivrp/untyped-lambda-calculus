{-# LANGUAGE ImportQualifiedPost #-}

module Untyped.Env
  ( Env (..),
    empty,
    lookup,
    extend,
  )
where

import Data.Map qualified as M
import Prelude hiding (lookup)

newtype Env a = Env (M.Map String a)
  deriving (Show)

empty :: Env a
empty = Env M.empty

lookup :: String -> Env a -> Maybe a
lookup key (Env env) = M.lookup key env

extend :: String -> a -> Env a -> Env a
extend key value (Env env) = Env $ M.insert key value env
