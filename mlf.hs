{-# LANGUAGE
    DeriveDataTypeable
  , LambdaCase
  , RecordWildCards #-}
module Main (main) where

import Control.Applicative

import Data.ByteString.UTF8 as ByteString

import System.Console.CmdArgs
import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)

import Parse
import Parser
import Scope
import qualified Stream
import Supply
import qualified Type.Restricted as Restricted
import Var

data MLF
  = Parse { string :: String }
  | Echo { string :: String }
  | Permissions { string :: String } deriving (Typeable, Data)

mlf :: String -> MLF
mlf progName =
  modes [ Parse { string = def &= argPos 0 &= typ "STRING" }
        , Echo { string = def &= argPos 0 &= typ "STRING" }
        , Permissions { string = def &= argPos 0 &= typ "STRING" }
        ] &= program progName

main :: IO ()
main = do
  mlf <$> getProgName >>= cmdArgs >>= \ case
    Parse {..} -> case runParser parse (ByteString.fromString string) of
      Right t -> print t
      Left e -> do
        hPrint stderr e
        exitFailure
    Echo {..} -> case runParser parse (ByteString.fromString string) of
      Right t ->
        print $
        flip runSupply (flip Var Nothing <$> Stream.enumFrom zero) $
        Restricted.fromSyntactic =<<
        scope t
      Left e -> do
        hPrint stderr e
        exitFailure

zero :: Integer
zero = 0
