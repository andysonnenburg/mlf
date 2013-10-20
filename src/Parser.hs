{-# LANGUAGE LambdaCase #-}
module Parser
       ( Parser
       , runParser
       , ParserState (..)
       , ParseError (..)
       ) where

import Control.Monad.State.Strict

import Data.ByteString (ByteString)
import Data.Text.Encoding.Error

import Text.PrettyPrint.Free

import Loc
import Product
import Sum
import Token

type Parser = StateT ParserState (Sum (Product Loc ParseError))

runParser :: Parser a -> ByteString -> Sum (Product Loc ParseError) a
runParser m xs = evalStateT m (ParserState (Pos 1 1) xs)

data ParserState = ParserState {-# UNPACK #-} !Pos {-# UNPACK #-} !ByteString

data ParseError
  = LexError
  | UnicodeException UnicodeException
  | UnexpectedToken Token deriving Show

instance Pretty ParseError where
  pretty = \ case
    LexError -> text "lex" <+> text "error"
    UnicodeException e -> text $ show e
    UnexpectedToken t -> text "unexpected" <+> pretty t
