module HigherOrder (module HigherOrder) where

import Lists
import Data.Maybe

lengthViaFoldl :: [a] -> Int
lengthViaFoldl list = Lists.foldl list 0 (\ sum _ -> sum + 1)

lengthViaFoldr :: [a] -> Int
lengthViaFoldr list = Lists.foldr list 0 (\ _ sum -> sum + 1)

firstViaFoldl :: [a] -> a
firstViaFoldl list = fromJust $ Lists.foldl list Nothing (\ first next -> Just $ fromMaybe next first)

firstViaFoldr :: [a] -> a
firstViaFoldr list = fromJust $ Lists.foldr list Nothing (\ next _ -> Just next)

lastViaFoldl :: [a] -> a
lastViaFoldl list = fromJust $ Lists.foldl list Nothing (\ _ next -> Just next)

lastViaFoldr :: [a] -> a
lastViaFoldr list = fromJust $ Lists.foldr list Nothing (\ next last -> Just $ fromMaybe next last)

