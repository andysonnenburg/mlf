module Parser
       ( Parser
       , runParser
       , S (..)
       , ParseError (..)
       ) where

import Control.Monad.State.Strict

import Data.ByteString (ByteString)
import Data.Text.Encoding.Error

import Loc
import Token

type Parser = StateT S (Either (Loc, ParseError))

runParser :: Parser a -> ByteString -> Either (Loc, ParseError) a
runParser m xs = evalStateT m S { pos = Pos 1 1, input = xs }

data S = S { pos :: {-# UNPACK #-} !Pos, input :: {-# UNPACK #-} !ByteString }

data ParseError
  = LexError
  | UnicodeException UnicodeException
  | UnexpectedToken Token deriving Show
