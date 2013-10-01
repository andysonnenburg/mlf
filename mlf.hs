{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , LambdaCase
  , RecordWildCards
  , TupleSections
  , TypeFamilies #-}
module Main (main) where

import Control.Applicative
import Control.Comonad.Env (ComonadEnv, ask, extract)
import Control.Monad.Error (MonadError)
import Control.Monad.ST.Safe
import Control.Monad.State.Strict

import Data.ByteString.UTF8 as ByteString
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
import Name
import Parse
import Parser
import Product (local)
import Rename
import ST
import qualified Stream
import Sum
import Supply
import Type.Graphic (Node (Node), NodeSet, forNodeSet_)
import qualified Type.Graphic as Graphic
import Type.Permission (getPermissions)
import qualified Type.Permission as Permission
import qualified Type.Restricted as Restricted
import Unify
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
         ts <- getNodeSets t_g names
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

getNodeSets :: (MonadST m, s ~ World m)
            => NodeSet s Text -> [String] -> m [NodeSet s Text]
getNodeSets t_n0 xs = flip execStateT mempty $ forNodeSet_ t_n0 $ \ t_n ->
  find t_n >>= read >>= \ case
    Node (Name (Just y) _) _ _ | Set.member y ys -> modify (t_n:)
    _ -> return ()
  where
    ys = Set.fromList $ map Text.pack xs

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
