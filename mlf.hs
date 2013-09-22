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
import Data.Function (fix)
import Data.IntMap.Strict ((!))

import System.Console.CmdArgs
import System.Console.Terminfo.PrettyPrint
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)

import Text.PrettyPrint.Free (pretty, putDoc)

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
import Type.Permission (getPermissions, toScopedEffect)
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
        hPrint stderr e
        exitFailure
      R a -> do
        putDoc $ pretty a
        putStrLn ""
  Permissions {..} ->
    do t <- throws ParseError . runParser parse $ ByteString.fromString input
       runST $ flip runSupplyT (Stream.enumFrom 0) $ runSumT $ do
         t_r <- Restricted.fromSyntactic =<< throws RenameError (rename t)
         t_g <- Graphic.fromRestricted t_r
         p <- fmap toScopedEffect <$> getPermissions t_g
         t' <- Graphic.toSyntactic t_g
         return $ mapEnv (p!) t'
    |> \ case
      L e -> do
        hPrint stderr e
        exitFailure
      R a -> displayLn a

mapEnv :: (a -> b) ->
          Product a (PolyType (Product a) c) ->
          Product b (PolyType (Product b) c)
mapEnv f = local f . fmap mapPoly
  where
    mapPoly = fix $ \ rec -> \ case
      Mono w -> Mono $ local f $ mapMono <$> w
      Bot -> Bot
      Forall a bf w w' -> Forall a bf (local f (rec <$> w)) (local f (rec <$> w'))
    mapMono = fix $ \ rec -> \ case
      Var a -> Var a
      Arr w w' -> Arr (local f (rec <$> w)) (local f (rec <$> w'))

data Error e a
  = ParseError e ParseError
  | RenameError e (RenameError a) deriving Show

throws :: (MonadCatch (Product a b) m n, MonadError e n) => (a -> b -> e) -> m c -> n c
throws f = mapE (f <$> ask <*> extract)
