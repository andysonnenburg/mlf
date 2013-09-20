{
{-# LANGUAGE ViewPatterns #-}
module Parse (parse) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Comonad
import Control.Comonad.Env
import Control.Monad.Trans.Class

import Data.Text (Text)

import Lex
import Loc
import Parser
import Token (Token)
import qualified Token
import Type.Syntactic

import Prelude hiding (lex)
}

%name polyType

%monad { Parser } { >>= } { return }
%lexer { lexer } { (extract -> Token.EOF) } 
%error { parseError }

%tokentype { (Loc, Token) }

%token
  FORALL { (extract -> Token.Forall) }
  '->' { (extract -> Token.Arrow) }
  '_|_' { (extract -> Token.Bottom) }
  '>' { (extract -> Token.Flexible) }
  '=' { (extract -> Token.Rigid) }
  '(' { (extract -> Token.LeftParen) }
  ')' { (extract -> Token.RightParen) }
  VAR { (extract -> Token.Var _) }

%right '->'

%%

polyType
  : monoType { Mono $1 }
  | '_|_' { Bot }
  | FORALL '(' var '<>' polyType ')' polyType { Forall $3 $4 $5 $7 }

monoType
  : var { Var $1 }
  | monoType '->' monoType { Arr $1 $3 }
  | '(' monoType ')' { $2 }

var : VAR { case extract $1 of Token.Var x -> x }

'<>'
  : '>' { Flexible }
  | '=' { Rigid }

{
infixl 4 <%>, <%

parse :: Parser (PolyType Text)
parse = polyType

parseError :: (Loc, Token) -> Parser a
parseError = lift <<< Left <<< (,) <$> ask <*> UnexpectedToken . extract

lexer :: ((Loc, Token) -> Parser a) -> Parser a
lexer = (lex >>=)

(<%>) :: Functor f => (a -> b) -> f a -> f b
(<%>) = (<$>)

(<%) :: Functor f => a -> f b -> f a
(<%) = (<$)
}
