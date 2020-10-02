module Functors (module Functors) where

import Classes
import Data.Char

instance Functor Tree where
	fmap mapping tree = case tree of
		Leaf leaf -> Leaf $ mapping leaf
		Node value rest -> Node (mapping value) $ map (fmap mapping) rest

data CustomFunctor options x = CustomFunctor ((->) options x)
instance Functor (CustomFunctor options) where
	fmap = undefined

data Parser a = Parser (String -> [(a, String)])

char :: Parser Char
char = Parser $ \ string -> case string of
	[] -> []
	(first : rest) -> [(first, rest)]

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \ string -> case string of
	[] -> []
	(first : rest) -> case predicate first of
		False -> []
		True  -> [(first : rest)]

digit :: Parser Int
digit = toDigit <$> satisfy (\ char -> char `elem` "0123456789")
	where toDigit x = ord x - ord '0'
