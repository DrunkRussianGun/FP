module Types
( List(..)
, fromMaybe
, len
, Body(..)
, volume
, PointD(..)
, Tree(..)
, safeLeft
, safeRight
, depth
, Types.sum
, ArithmeticExpression
, calculate
, openParenthesis
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

data NaivePointD = NaivePointD Double Double
getX (NaivePointD x _) = x
getY (NaivePointD _ y) = y

data PointD = PointD
	{ x :: Double
	, y :: Double }

data Tree a
	= Leaf { value :: a }
	| Branch
		{ left :: Tree a
		, value :: a
		, right :: Tree a }

safeLeft :: Tree a -> Maybe (Tree a)
safeLeft (Leaf _) = Nothing
safeLeft (Branch left _ _) = Just left

safeRight :: Tree a -> Maybe (Tree a)
safeRight (Leaf _) = Nothing
safeRight (Branch _ _ right) = Just right

depth :: Tree a -> Int
depth (Leaf _) = 0
depth (Branch left _ right) = depth left `max` depth right + 1

sum :: Tree Int -> Int
sum (Leaf value) = value
sum (Branch left value right) = Types.sum left + value + Types.sum right

data ArithmeticExpression
	= Const Int
	| Plus ArithmeticExpression ArithmeticExpression
	| Mult ArithmeticExpression ArithmeticExpression
	deriving (Show)

calculate :: ArithmeticExpression -> Int
calculate (Const value) = value
calculate (Plus left right) = calculate left + calculate right
calculate (Mult left right) = calculate left * calculate right

openParenthesis :: ArithmeticExpression -> ArithmeticExpression
openParenthesis expr = case expr of
	Const value -> Const value
	left `Plus` right -> openParenthesis left `Plus` openParenthesis right
	(left `Plus` right) `Mult` multiplier ->
		(openParenthesis left `Mult` openParenthesis multiplier)
		`Plus`
		(openParenthesis right `Mult` openParenthesis multiplier)
	multiplier `Mult` (left `Plus` right) ->
		(openParenthesis multiplier `Mult` openParenthesis left)
		`Plus`
		(openParenthesis multiplier `Mult` openParenthesis right)
	left `Mult` right -> openParenthesis left `Mult` openParenthesis right
