{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , RecordWildCards #-}
module Main (main) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Monad
import Control.Monad.Error hiding (Error (..))
import Control.Monad.ST.Safe

import Data.ByteString.UTF8 as ByteString
import Data.Text

import System.Console.CmdArgs
import System.Environment (getProgName)

import Hoist
import Loc
import Parse
import Parser
import Rename
import qualified Stream
import Supply
import qualified Type.Graphic as Graphic
import qualified Type.Restricted as Restricted
import Unify

data MLF
  = Echo { input :: String }
  | Unify { input :: String } deriving (Typeable, Data)

mlf :: String -> MLF
mlf progName =
  modes [ Echo (def &= argPos 0 &= typ "STRING")
        , Unify (def &= argPos 0 &= typ "STRING")
        ] &= program progName

main :: IO ()
main = mlf <$> getProgName >>= cmdArgs >>= \ case
  Echo {..} ->
    print <<<
    fmap (\ t -> runST (Graphic.toSyntactic =<< Graphic.fromRestricted t)) <<<
    flip runSupplyT (Stream.enumFrom 0) <<<
    Restricted.fromSyntactic <=<
    throwsRenameError . rename <=<
    throwsParseError . runParser parse $
    ByteString.fromString input

data Error
  = ParseError !Loc !ParseError
  | RenameError !(RenameError Text) deriving Show

newtype Compiler s a = Compiler { unCompiler :: Either Error a } deriving Show

instance Functor (Compiler s) where
  fmap f = Compiler . fmap f . unCompiler

instance Applicative (Compiler s) where
  pure = Compiler . pure
  f <*> a = Compiler $ unCompiler f <*> unCompiler a

instance Monad (Compiler s) where
  return = Compiler . return
  m >>= f = Compiler $ unCompiler m >>= unCompiler . f

instance MonadError (Loc, ParseError) (Compiler (Loc, ParseError)) where
  throwError = Compiler . Left . uncurry ParseError
  m `catchError` h = case unCompiler m of
    Left (ParseError l e) -> h (l, e)
    _ -> m

instance MonadError (RenameError Text) (Compiler (RenameError Text)) where
  throwError = Compiler . Left . RenameError
  m `catchError` h = case unCompiler m of
    Left (RenameError e) -> h e
    _ -> m

throwsRenameError :: MonadHoist t => t (Compiler (RenameError Text)) a -> t (Compiler s) a
throwsRenameError = hoist (Compiler . unCompiler)

throwsParseError :: MonadTrans t => Either (Loc, ParseError) a -> t (Compiler s) a
throwsParseError = lift . Compiler . mapLeft (uncurry ParseError)

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = \ case
  Left e -> Left (f e)
  Right a -> Right a
