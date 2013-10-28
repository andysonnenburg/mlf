{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , LambdaCase
  , RecordWildCards
  , TypeFamilies #-}
module Main (main) where

import Control.Applicative
import Control.Category ((>>>))
import Control.Comonad.Env (ComonadEnv, ask, extract)
import Control.Monad.Error (MonadError)
import Control.Monad.ST.Safe
import Control.Monad.State.Strict

import Data.ByteString.UTF8 as ByteString
import Data.Foldable (foldlM)
import qualified Data.HashSet as Set
import Data.IntMap.Strict ((!))
import Data.Monoid (mempty)
import Data.Text (Text)
import qualified Data.Text as Text

import System.Console.CmdArgs
import System.Console.Terminfo.PrettyPrint
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Text.PrettyPrint.Free

import Prelude hiding (read)

import Catch
import Function
import Hoist
import IntMap (toIntMap)
import Parse
import Parser
import Product (local)
import Rename
import ST
import qualified Stream
import Sum
import Supply
import Type.Graphic (Bound (Bound), preorder, project)
import qualified Type.Graphic as Graphic
import Type.Permission (getPermissions)
import qualified Type.Permission as Permission
import qualified Type.Restricted as Restricted
import Type.Unify
import UnionFind

data MLF
  = Echo { input :: String }
  | Permissions { input :: String }
  | Unify { input :: String, names :: [String] } deriving (Typeable, Data)

mlf :: String -> MLF
mlf progName =
  modes [ Echo (def &= argPos 0 &= typ "TYPE")
        , Permissions (def &= argPos 0 &= typ "TYPE")
        , Unify (def &= argPos 0 &= typ "TYPE") (def &= args &= typ "VAR")
        ] &= program progName

main :: IO ()
main = mlf <$> getProgName >>= cmdArgs >>= \ case
  Echo {..} ->
    do t <- mapEE ParseError . runParser parse $ ByteString.fromString input
       runST $ flip runSupplyT (Stream.enumFrom 0) $ runSumT $ do
         t_r <- Restricted.fromSyntactic =<< mapEE RenameError (rename t)
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
    do t <- mapEE ParseError . runParser parse $ ByteString.fromString input
       runST $ flip runSupplyT (Stream.enumFrom 0) $ runSumT $ do
         t_r <- Restricted.fromSyntactic =<< mapEE RenameError (rename t)
         t_g <- Graphic.fromRestricted t_r
         p <- toIntMap . fmap Permission.toScopedEffect <$> getPermissions t_g
         hoist' (local (p!)) <$> Graphic.toSyntactic t_g
    |> \ case
      L e -> do
        hPutDoc stderr $ pretty e
        hPutStrLn stderr ""
        exitFailure
      R a -> displayLn a
  Unify {..} ->
    do t <- mapEE ParseError . runParser parse $ ByteString.fromString input
       runST $ flip runSupplyT (Stream.enumFrom 0) $ runSumT $ do
         t_r <- Restricted.fromSyntactic =<< mapEE RenameError (rename t)
         t_g <- Graphic.fromRestricted t_r
         ts <- getTypes names t_g
         mapE UnifyError (unify t_g ts)
         Graphic.toSyntactic t_g
    |> \ case
      L e -> do
        hPutDoc stderr $ pretty e
        hPutStrLn stderr ""
        exitFailure
      R a -> do
        putDoc $ pretty a
        putStrLn ""

getTypes :: (MonadST m, s ~ World m)
         => [String]
         -> Graphic.Type s (Maybe Text)
         -> m [Graphic.Type s (Maybe Text)]
getTypes xs = find >=> read >=> preorder >=> foldlM (\ ts t ->
  find t >>= read |> fmap $ project >>> \ case
    Bound (Just y) _ _ | Set.member y ys -> t:ts
    _ -> ts) mempty
  where
    ys = Set.fromList $ Text.pack <$> xs

data Error e a
  = ParseError e ParseError
  | RenameError e (RenameError a)
  | UnifyError (UnifyError a) deriving Show

instance ( Pretty e
         , Pretty a
         ) => Pretty (Error e a) where
  pretty = \ case
    ParseError e a -> pretty e <> char ':' <+> pretty a
    RenameError e a -> pretty e <> char ':' <+> pretty a
    UnifyError a -> pretty a

mapEE :: (ComonadEnv a w, MonadCatch (w b) m n, MonadError e n)
      => (a -> b -> e) -> m c -> n c
mapEE f = mapE (f <$> ask <*> extract)
