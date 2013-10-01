{
{-# LANGUAGE ViewPatterns #-}
module Parse (parse) where

import Control.Applicative
import Control.Category ((<<<))
import Control.Comonad
import Control.Comonad.Env
import Control.Monad.Error

import Data.Text (Text)

import Lex
import Loc
import Parser
import Product
import Token (Token)
import qualified Token
import Type.Syntactic

import Prelude hiding (lex)
}

%name polyType

%monad { Parser } { >>= } { return }
%lexer { lexer } { (extract -> Token.EOF) } 
%error { parseError }

%tokentype { Product Loc Token }

%token
  '->' { (extract -> Token.Arr) }
  '_|_' { (extract -> Token.Bot) }
  '>' { (extract -> Token.Flexible) }
  '=' { (extract -> Token.Rigid) }
  '(' { (extract -> Token.LeftParen) }
  ')' { (extract -> Token.RightParen) }
  VAR { (extract -> Token.Var _) }

%right '->'

%%

polyType
  : monoType { Mono (extract $1) <% $1 }
  | '_|_' { Bot <% $1 }
  | '(' var '<>' polyType ')' polyType {
      Forall (extract $2) (extract $3) $4 $6 <% $1 <@ $2 <@ $3 <@ $4 <@ $5 <@ $6
    }

monoType
  : var { Var (extract $1) <% $1 }
  | monoType '->' monoType { Arr $1 $3 <% $1 <@ $2 <@ $3 }
  | '(' monoType ')' { $2 <@ $1 <@ $3 }

var : VAR { case extract $1 of Token.Var x -> x <% $1 }

'<>'
  : '>' { Flexible <% $1 }
  | '=' { Rigid <% $1 }

{
infixl 4 <%>, <%

parse :: Parser (Product Loc (PolyType (Product Loc) Text))
parse = polyType

parseError :: Product Loc Token -> Parser a
parseError = throwError . fmap UnexpectedToken

lexer :: (Product Loc Token -> Parser a) -> Parser a
lexer = (lex >>=)

(<%>) :: Functor f => (a -> b) -> f a -> f b
(<%>) = (<$>)

(<%) :: Functor f => a -> f b -> f a
(<%) = (<$)
}
