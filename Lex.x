{
{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
{-# OPTIONS_GHC
    -fno-warn-lazy-unlifted-bindings
    -fno-warn-missing-signatures
    -fno-warn-unused-binds
    -fno-warn-unused-imports
    -fno-warn-unused-matches #-}
module Lex (lex) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State.Strict

import Data.ByteString.Internal (w2c)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)

import Loc
import Parser
import Token

import Prelude hiding (lex)
}

$lowercase = [a-z_]
$uppercase = [A-Z]
$numeric = [0-9]

@var = $lowercase [$lowercase $uppercase $numeric]*

:-

$white+ ;

"forall" { forall }
"->" { arrow }
"_|_" { bottom }
">" { flexible }
"=" { rigid }
"(" { leftParen }
")" { rightParen }
@var { var }

{
lex :: Parser (Loc, Token)
lex = do
  s@S { pos, input } <- get
  case alexScan s 0 of
    AlexEOF -> return (Loc pos pos, EOF)
    AlexError S { pos = pos' } -> lift $ Left (Loc pos pos', LexError)
    AlexSkip s' _ -> put s' >> lex
    AlexToken s'@S { pos = pos' } n m -> put s' >> m (Loc pos pos') input n

type Action = Loc -> ByteString -> Int -> Parser (Loc, Token)

forall :: Action
forall loc _ _ = return (loc, Forall)

arrow :: Action
arrow loc _ _ = return (loc, Arrow)

bottom :: Action
bottom loc _ _ = return (loc, Bottom)

flexible :: Action
flexible loc _ _ = return (loc, Flexible)

rigid :: Action
rigid loc _ _ = return (loc, Rigid)

leftParen :: Action
leftParen loc _ _ = return (loc, LeftParen)

rightParen :: Action
rightParen loc _ _ = return (loc, RightParen)

var :: Action
var loc xs n = do
  xs' <- fromLazyByteString $ ByteString.take (fromIntegral n) xs
  return (loc, Var xs')

fromLazyByteString :: ByteString -> Parser Text
fromLazyByteString =
  either (\ e -> get >>= \ S {..} -> lift $ Left (Loc pos pos, UnicodeException e))
  return .
  Text.decodeUtf8'

type AlexInput = S

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte S {..} =
  (\ (x, xs) -> (x, S { pos = plusPos (w2c x) pos, input = xs})) <$>
  ByteString.uncons input
}
