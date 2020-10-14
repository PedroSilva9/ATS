module ParserLet where

import Parser
import Data.Char
import Prelude hiding ((<*>), (<$>), exp)

data Let = Let Items Exp
    deriving Show

type Items = [Item]

data Item = NestedLet String Let
          | Atrib String Exp
          deriving Show

type Exp = String

plet = f <$> token' "let" <*> enclosedBy (symbol' '{') items (symbol' '}')
     <*> token' "in" <*> exp
    where
        f a b c d = Let b d

items' = separatedBy item (symbol' ';')

items = singl <$> item
      <|> f <$> item <*> symbol ';' <*> items
    where
        f a b c = a : c

item = f <$> ident <*> symbol' '=' <*> exp
     <|> g <$> ident <*> symbol' '=' <*> plet
    where
        f a b c = Atrib a c
        g a b c = NestedLet a c

exp = ident
    <|> pNat

ident = f <$> oneOrMore (satisfy isAlpha) <*> spaces
    where
        f a b = a

pNat = (\a b -> a) <$> oneOrMore (satisfy isDigit) <*> spaces
