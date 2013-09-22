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
import Control.Comonad.Env
import Control.Monad.Error
import Control.Monad.State.Strict

import Data.ByteString.Internal (w2c)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import Data.Word (Word8)

import Prelude hiding (lex)

import Loc
import Parser
import Product
import Token
}

$lowercase = [a-z_]
$uppercase = [A-Z]
$numeric = [0-9]

@var = $lowercase [$lowercase $uppercase $numeric]*

:-

$white+ ;

"forall" { forall }
"->" { arr' }
"_|_" { bot }
">" { flexible }
"=" { rigid }
"(" { leftParen }
")" { rightParen }
@var { var }

{
lex :: Parser (Product Loc Token)
lex = do
  s@(ParserState pos xs) <- get
  case alexScan s 0 of
    AlexEOF -> return $ Loc pos pos :* EOF
    AlexError (ParserState pos' _) -> throwError $ Loc pos pos' :* LexError
    AlexSkip s' _ -> put s' >> lex
    AlexToken s'@(ParserState pos' _) n m -> put s' >> m (Loc pos pos') xs n

type Action = Loc -> ByteString -> Int -> Parser (Product Loc Token)

forall :: Action
forall loc _ _ = return $ loc :* Forall

arr' :: Action
arr' loc _ _ = return $ loc :* Arr

bot :: Action
bot loc _ _ = return $ loc :* Bot

flexible :: Action
flexible loc _ _ = return $ loc :* Flexible

rigid :: Action
rigid loc _ _ = return $ loc :* Rigid

leftParen :: Action
leftParen loc _ _ = return $ loc :* LeftParen

rightParen :: Action
rightParen loc _ _ = return $ loc :* RightParen

var :: Action
var loc xs n = do
  xs' <- fromLazyByteString $ ByteString.take (fromIntegral n) xs
  return (loc :* Var xs')

fromLazyByteString :: ByteString -> Parser Text
fromLazyByteString =
  either (\ e -> get >>= \ (ParserState pos _) -> throwError (Loc pos pos :* UnicodeException e))
  return .
  Text.decodeUtf8'

type AlexInput = ParserState

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (ParserState pos xs) =
  (\ (x, ys) -> (x, ParserState (plusPos (w2c x) pos) ys)) <$>
  ByteString.uncons xs
}
