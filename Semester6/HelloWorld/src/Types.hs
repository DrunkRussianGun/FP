module Types
( List(..)
, fromMaybe
, len
, Body(..)
, volume
) where

data List a =
	Nil
	| Element a (List a)

len :: List a -> Int
len Nil = 0
len (Element _ list) = len list + 1

fromMaybe :: Maybe a -> a -> a
fromMaybe Nothing def = def 
fromMaybe (Just x) def = x

data Body = Body Int Double

volume (Body height weight)
	= fromIntegral height * weight