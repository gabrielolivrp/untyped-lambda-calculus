{-# LANGUAGE ImportQualifiedPost #-}

module Untyped.Interp (norm, beta) where

import Data.Set qualified as S
import Untyped.Syntax

fresh :: Name -> S.Set Name -> Name
fresh x xs = if x `S.member` xs then fresh (x <> "'") xs else x

-- | Set of free variables
--
-- FV(x) = {x}, where x is a variable.
-- FV(λx.M) = FV(M) \ {x}.
-- FV(M N) = FV(M) ∪ FV(N).
fv :: Term -> S.Set Name
fv (Var x) = S.singleton x
fv (Abs param body) = S.delete param (fv body)
fv (App func argm) = S.union (fv func) (fv argm)

-- | Substitution function
--
-- x[M/x] = M
-- c[M/x] = x, where x is any variable or constant = c
-- (E F)[M/x] = E[M/x] F[M/x]
-- (λx.E)[M/x] = λx.E
-- (λy.E)[M/x], where y os any variable other than x
--  > λy.E[M/x], if x does not occur free in E or y does not occur free in M
--  > λz.(E[z/y])E[M/x] otherwise, where z is a new variable name which does
--                                 not occur free in E or M
subst :: Term -> Name -> Term -> Term
subst var@(Var v) x t
  | x == v = t
  | otherwise = var
subst l@(Abs param body) x t
  | param == x = l
  | param /= x && param `S.notMember` fv t =
      Abs param (subst body x t)
  | otherwise =
      let z = fresh param (fv body `S.union` fv t)
       in subst (Abs z (subst body param (Var z))) x t
subst (App func argm) x s = App (subst func x s) (subst argm x s)

-- | Alpha equivalence
--
-- check if two λ-expressions are equivalent
alphaEq :: Term -> Term -> Bool
alphaEq (Var _) (Var _) = True
alphaEq (Abs _ body) (Abs _ body') = alphaEq body body'
alphaEq (App func argm) (App func' argm') = alphaEq func func' && alphaEq argm argm'
alphaEq _ _ = False

-- | Beta reduction
beta :: Term -> Term
beta (Var v) = Var v
beta (Abs param body) = Abs param (beta body)
beta (App (Abs param body) argm) = subst body param argm
beta (App func argm) = App (beta func) (beta argm)

-- | Normal form
norm :: Term -> Term
norm expr =
  let expr' = beta expr
   in if alphaEq expr' expr
        then expr'
        else norm expr'
