{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleInstances
  , LambdaCase
  , MultiParamTypeClasses
  , RecordWildCards
  , TypeFamilies
  , UndecidableInstances #-}
module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class hiding (Error (..))
import Control.Monad.ST.Safe
import Control.Monad.Trans.Class

import Data.ByteString.UTF8 as ByteString
import Data.Text

import System.Console.CmdArgs
import System.Environment (getProgName)

import Loc
import Parse
import Parser
import Rename
import ST
import qualified Stream
import Supply
import qualified Type.Graphic as Graphic
import qualified Type.Restricted as Restricted
import Unify

import Debug.Trace

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
  Echo {..} -> print $ do
    t <- throwsParseError . runParser parse $ ByteString.fromString input
    runST $ flip runSupplyT (Stream.enumFrom 0) $ runErrorT $ do
      t_r <- Restricted.fromSyntactic =<< throws RenameError (rename t)
      t_g <- Graphic.fromRestricted t_r
      trace (show (t, t_r)) $ liftST $ Graphic.toSyntactic t_g

data Error
  = ParseError !Loc !ParseError
  | RenameError !(RenameError Text) deriving Show

newtype ErrorT e m a = ErrorT { runErrorT :: m (Either e a) }

throws :: Functor m => (e -> e') -> ErrorT e m a -> ErrorT e' m a
throws f = ErrorT . fmap (mapLeft f) . runErrorT

instance Functor m => Functor (ErrorT e m) where
  fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Applicative m, Monad m) => Applicative (ErrorT e m) where
  pure = ErrorT . pure . Right
  f <*> a = ErrorT $ runErrorT f >>= \ case
    Left e -> pure (Left e)
    Right f' -> runErrorT a >>= \ case
      Left e -> pure (Left e)
      Right a' -> pure (Right (f' a'))

instance Monad m => Monad (ErrorT e m) where
  return = ErrorT . return . Right
  m >>= f = ErrorT $ runErrorT m >>= \ case
    Left e -> return (Left e)
    Right a -> runErrorT (f a)

instance MonadTrans (ErrorT e) where
  lift = ErrorT . liftM Right

instance Monad m => MonadError e (ErrorT e m) where
  throwError = ErrorT . return . Left
  m `catchError` h = ErrorT $ runErrorT m >>= \ case
    Left e -> runErrorT (h e)
    Right a -> return (Right a)

instance MonadST m => MonadST (ErrorT e m) where
  type World (ErrorT e m) = World m
instance MonadSupply s m => MonadSupply s (ErrorT e m)

throwsParseError :: Either (Loc, ParseError) a -> Either Error a
throwsParseError = mapLeft (uncurry ParseError)

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = \ case
  Left e -> Left (f e)
  Right a -> Right a
