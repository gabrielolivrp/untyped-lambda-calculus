{-# LANGUAGE OverloadedStrings #-}

module Untyped.Syntax
  ( Name,
    Term (..),
    TopLevel (..),
    Program,
    ppExpr,
  )
where

import Prettyprinter
import Prelude hiding ((<>))

type Name = String

data Term
  = Var Name
  | Abs Name Term
  | App Term Term
  deriving (Show, Eq)

data TopLevel
  = LetDecl Name Term
  deriving (Show, Eq)

type Program = [TopLevel]

instance Pretty Term where
  pretty (Var name) = pretty name
  pretty (Abs param body) =
    let body' = pretty body
     in "λ" <> pretty param <> "." <+> body'
  pretty (App func arg) =
    let expr = pretty func <+> pretty arg
     in parens expr

ppExpr :: Term -> String
ppExpr = show . pretty