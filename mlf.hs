{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , LambdaCase
  , RecordWildCards #-}
module Main (main) where

import Control.Applicative
import Control.Comonad.Env (ask, extract)
import Control.Monad.Error (MonadError)
import Control.Monad.ST.Safe

import Data.ByteString.UTF8 as ByteString
import Data.IntMap.Strict ((!))

import System.Console.CmdArgs
import System.Console.Terminfo.PrettyPrint
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.PrettyPrint.Free

import Catch
import Function
import Parse
import Parser
import Product
import Rename
import qualified Stream
import Sum
import Supply
import qualified Type.Graphic as Graphic
import Type.Permission (getPermissions)
import qualified Type.Permission as Permission
import qualified Type.Restricted as Restricted
import Type.Syntactic

data MLF
  = Echo { input :: String }
  | Permissions { input :: String }
  | Unify { input :: String } deriving (Typeable, Data)

mlf :: String -> MLF
mlf progName =
  modes [ Echo (def &= argPos 0 &= typ "STRING")
        , Permissions (def &= argPos 0 &= typ "STRING")
        , Unify (def &= argPos 0 &= typ "STRING")
        ] &= program progName

main :: IO ()
main = mlf <$> getProgName >>= cmdArgs >>= \ case
  Echo {..} ->
    do t <- throws ParseError . runParser parse $ ByteString.fromString input
       runST $ flip runSupplyT (Stream.enumFrom 0) $ runSumT $ do
         t_r <- Restricted.fromSyntactic =<< throws RenameError (rename t)
         t_g <- Graphic.fromRestricted t_r
         Graphic.toSyntactic t_g
    |> \ case
      L e -> do
        hPutDoc stderr $ pretty e
        hPutStrLn stderr ""
        exitFailure
      R a -> do
        putDoc $ pretty a
        putStrLn ""
  Permissions {..} ->
    do t <- throws ParseError . runParser parse $ ByteString.fromString input
       runST $ flip runSupplyT (Stream.enumFrom 0) $ runSumT $ do
         t_r <- Restricted.fromSyntactic =<< throws RenameError (rename t)
         t_g <- Graphic.fromRestricted t_r
         p <- fmap Permission.toScopedEffect <$> getPermissions t_g
         mapEnv (p!) <$> Graphic.toSyntactic t_g
    |> \ case
      L e -> do
        hPutDoc stderr $ pretty e
        hPutStrLn stderr ""
        exitFailure
      R a -> displayLn a

data Error e a
  = ParseError e ParseError
  | RenameError e (RenameError a) deriving Show

instance ( Pretty e
         , Pretty a
         ) => Pretty (Error e a) where
  pretty = \ case
    ParseError e a -> pretty e <> char ':' <+> pretty a
    RenameError e a -> pretty e <> char ':' <+> pretty a

throws :: (MonadCatch (Product a b) m n, MonadError e n) => (a -> b -> e) -> m c -> n c
throws f = mapE (f <$> ask <*> extract)
