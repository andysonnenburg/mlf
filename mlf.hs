{-# LANGUAGE
    DeriveDataTypeable
  , LambdaCase
  , RecordWildCards#-}
module Main (main) where

import Control.Applicative
import Control.Monad.ST.Safe

import Data.ByteString.UTF8 as ByteString
import Data.Text

import System.Console.CmdArgs
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)

import Text.PrettyPrint.Free (pretty, putDoc)

import Error
import Loc
import Parse
import Parser
import Rename
import ST
import qualified Stream
import Supply
import qualified Type.Graphic as Graphic
import qualified Type.Restricted as Restricted

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
  Echo {..} -> case do
    t <- throwsParseError . runParser parse $ ByteString.fromString input
    runST $ flip runSupplyT (Stream.enumFrom 0) $ runErrorT $ do
      t_r <- Restricted.fromSyntactic =<< throws RenameError (rename t)
      t_g <- Graphic.fromRestricted t_r
      liftST $ Graphic.toSyntactic t_g of
                 Left e -> do
                   hPrint stderr e
                   exitFailure
                 Right a -> do
                   putDoc $ pretty a
                   putStrLn ""

data Error
  = ParseError !Loc !ParseError
  | RenameError !(RenameError Text) deriving Show

throwsParseError :: Either (Loc, ParseError) a -> Either Error a
throwsParseError = mapLeft (uncurry ParseError)

throws :: Functor m => (e -> e') -> ErrorT e m a -> ErrorT e' m a
throws f = ErrorT . fmap (mapLeft f) . runErrorT

mapLeft :: (e -> e') -> Either e a -> Either e' a
mapLeft f = \ case
  Left e -> Left (f e)
  Right a -> Right a
