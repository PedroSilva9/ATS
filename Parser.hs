module Parser where

import Data.Char

import Prelude hiding ((<*>), (<$>))

infixl 2 <|>
infixl 3 <*>

type Parser s r = [s] -> [(r,[s])]

symbola :: Parser Char Char
symbola [] = []
symbola (x:xs) | x == 'a' = [('a', xs)]
               | otherwise = []

symbol :: Char -> Parser Char Char
symbol s [] = []
symbol s (x:xs) | x == s = [(s,xs)]
                | otherwise = []

satisfy :: (Char -> Bool) -> Parser Char Char
satisfy p [] = []
satisfy p (x:xs) | p x = [(x,xs)]
                 | otherwise = []

token :: [Char] -> Parser Char [Char]
token t [] = []
token t inp | (take (length t) inp) == t = [(t, drop (length t) inp)]
            | otherwise = []

succed r inp = [(r, inp)]


(<|>) :: Parser s a -> Parser s a -> Parser s a
(p <|> q) inp = p inp ++ q inp

-- (<**>) :: Parser s a -> Parser s b -> Parser s (a,b)
-- (p <**> q) inp = [ ((r, r'), ys)
--                  | (r,  xs) <- p inp
--                  , (r', ys) <- q xs
--                  ]

-- exSeq = symbol 'a' <**> satisfy isDigit <**> token "for" <**> symbol ' '

(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
(p <*> q) inp = [ (f r, ys)
                | (f, xs) <- p inp
                , (r, ys) <- q xs
                ]

(<$>) :: (a -> r) -> Parser s a -> Parser s r
(f <$> p) inp = [ (f r, inp')
                | (r,   inp') <- p inp
                ]

pSeq = f <$>  symbol 'a' <*> symbol ',' <*> symbol 'b' <|> g <$> symbol 'c'
    where
        f a1 a2 a3 = [a1,a3]
        g a1       = [a1]

spaces = (:) <$> satisfy isSpace <*> spaces
       <|> succed []

zeroOrMore p = (:) <$> p <*> zeroOrMore p
             <|> succed []

spaces' = zeroOrMore (satisfy isSpace)

singl = (: [])

oneOrMore p = (:) <$> p <*> oneOrMore p
            <|> singl <$> p

token' t = curry fst <$> token t <*> spaces

symbol' t = curry fst <$> symbol t <*> spaces

separatedBy p s = singl <$> p
                <|> g <$> p <*> s <*> separatedBy p s
    where
        g a b c = a:c

enclosedBy a b f = (\a b c -> b) <$> a <*> b <*> f
