{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RecordWildCards #-}

module REPL (runRepl) where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.State
import System.Console.Haskeline
import Untyped
import Untyped.Env qualified as E

type Context = E.Env Term

newtype ReplState = ReplState
  { ctx :: Context
  }

newtype Repl a = Repl
  { unR :: StateT ReplState IO a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState ReplState,
      MonadThrow,
      MonadCatch,
      MonadMask
    )

substitute :: Term -> Context -> Term
substitute v@(Var name) ctx =
  case E.lookup name ctx of
    Just expr -> substitute expr ctx
    Nothing -> v
substitute (Abs param body) ctx =
  Abs param (substitute body ctx)
substitute (App func argm) ctx =
  App (substitute func ctx) (substitute argm ctx)

evalRepl :: String -> InputT Repl ()
evalRepl input = do
  ReplState {..} <- lift get
  case parseExpr input of
    Left _ ->
      case parseTopLevel input of
        Left err -> (outputStrLn . show) err
        Right (LetDecl name expr) ->
          let ctx' = E.extend name expr ctx
           in lift $ modify (\s -> s {ctx = ctx'})
    Right expr ->
      let value = norm $ substitute expr ctx
       in (outputStrLn . ppExpr) value

loop :: InputT Repl ()
loop = do
  input <- getInputLine "Untyped> "
  case input of
    Nothing -> exit
    Just ":q" -> exit
    Just input' -> evalRepl input' >> loop

exit :: InputT Repl ()
exit = outputStrLn "Exit."

runRepl :: Context -> IO ()
runRepl ctx =
  let st = ReplState ctx
      repl = runInputT defaultSettings loop
   in void $ runStateT (unR repl) st
