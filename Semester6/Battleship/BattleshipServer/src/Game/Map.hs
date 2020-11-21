module Game.Map
	( Cell(..)
	, fromLeft
	, fromTop
	, plus
	, minus
	, CellRange
	, rangeLeftTop
	, rangeRightBottom
	, createRange
	, rangeLeft
	, rangeTop
	, rangeRight
	, rangeBottom
	, rangeWidth
	, rangeHeight
	, rangeIntersect
	, rangeContains
	, Direction(..)
	, Ship
	, shipBounds
	, shipAliveCells
	, createShip1
	, createShip2
	, Map
	, mapBounds
	, mapShips
	, createMap
	) where

import Data.List
import Data.Maybe
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

data Cell = Cell
	{ cellLeft :: Int
	, cellTop :: Int
	}
	deriving (Eq, Show, Read)

fromLeft :: Int -> Cell
fromLeft left = Cell left 0

{-# ANN fromTop "HLint: ignore Eta reduce" #-}
fromTop :: Int -> Cell
fromTop top = Cell 0 top

plus :: Cell -> Cell -> Cell
first `plus` second = Cell (cellLeft first + cellLeft second) (cellTop first + cellTop second)

minus :: Cell -> Cell -> Cell
first `minus` second = Cell (cellLeft first - cellLeft second) (cellTop first - cellTop second)

data CellRange = CellRange
	{ rangeLeftTop :: Cell
	, rangeRightBottom :: Cell 
	}
	deriving (Eq, Show, Read)

createRange :: Cell -> Cell -> Maybe CellRange
createRange leftTop rightBottom
	| cellLeft leftTop <= cellLeft rightBottom && cellTop leftTop <= cellTop rightBottom =
		Just $ CellRange leftTop rightBottom
	| otherwise = Nothing

rangeLeft :: CellRange -> Int
rangeLeft = cellLeft . rangeLeftTop

rangeTop :: CellRange -> Int
rangeTop = cellTop . rangeLeftTop

rangeRight :: CellRange -> Int
rangeRight = cellLeft . rangeRightBottom

rangeBottom :: CellRange -> Int
rangeBottom = cellTop . rangeRightBottom

rangeWidth :: CellRange -> Int
rangeWidth cellRange = rangeRight cellRange - rangeLeft cellRange + 1

rangeHeight :: CellRange -> Int
rangeHeight cellRange = rangeBottom cellRange - rangeTop cellRange + 1

rangeIntersect :: CellRange -> CellRange -> Maybe CellRange
rangeIntersect first second = let
	horizontalIntersect =
		intervalIntersect (rangeLeft first, rangeRight first) (rangeLeft second, rangeRight second)
	verticalIntersect =
		intervalIntersect (rangeTop first, rangeBottom first) (rangeTop second, rangeBottom second)
	intervalIntersect (firstLeft, firstRight) (secondLeft, secondRight)
		| secondRight < firstLeft || firstRight < secondLeft = Nothing
		| otherwise = Just (max firstLeft secondLeft, min firstRight secondRight)
	rangeFromIntervals (left, right) (top, bottom) = createRange (Cell left top) (Cell right bottom)
	in case isNothing horizontalIntersect || isNothing verticalIntersect of
		False -> rangeFromIntervals (fromJust horizontalIntersect) (fromJust verticalIntersect)
		True  -> Nothing

class RangeContains a where
	rangeContains :: CellRange -> a -> Bool

instance RangeContains CellRange where
	rangeContains range subrange
		= rangeLeft range <= rangeLeft subrange
		&& rangeRight range >= rangeRight subrange
		&& rangeTop range <= rangeTop subrange
		&& rangeBottom range >= rangeBottom subrange

instance RangeContains Cell where
	rangeContains range cell = rangeContains range $ CellRange cell cell

data Direction = Horizontal | Vertical
	deriving (Eq, Show, Read)

data Ship = Ship
	{ shipBounds :: CellRange
	, shipAliveCells :: IntSet -- ^ Номера «живых» ячеек, начиная с 0
	}
	deriving (Show, Read)

createShip1 :: CellRange -> Maybe Ship
createShip1 cellArea
	| rangeLeft cellArea == rangeRight cellArea = Just $ Ship cellArea $ IntSet.fromList [0..rangeHeight cellArea - 1]
	| rangeTop cellArea == rangeBottom cellArea = Just $ Ship cellArea $ IntSet.fromList [0..rangeWidth cellArea - 1]
	| otherwise = Nothing

createShip2 :: Cell -> Direction -> Int -> Maybe Ship
createShip2 leftTop direction length
	| length > 0 = case direction of
		Horizontal -> Just $ Ship (fromRightBottom $ leftTop `plus` fromLeft (length - 1)) aliveCells
		Vertical -> Just $ Ship (fromRightBottom $ leftTop `plus` fromTop (length - 1)) aliveCells
	| otherwise  = Nothing
		where fromRightBottom :: Cell -> CellRange
		      fromRightBottom rightBottom = fromJust $ createRange leftTop rightBottom
		      aliveCells = IntSet.fromList [0..length - 1]

data Map = Map
	{ mapBounds :: CellRange
	, mapShips :: [Ship]
	}
	deriving (Show, Read)

createMap :: CellRange -> [Ship] -> Maybe Map
createMap bounds ships
	| length ships > 0
		&& (all containedInMap $ map shipBounds ships)
		&& (all shipPairNotIntersects $ pairs ships)
		= Just $ Map bounds ships
	| otherwise = Nothing
		where containedInMap range = bounds `rangeContains` range
		      pairs list
		      	= [(tailHead, second) | (tailHead : restOfTail) <- tails list, second <- restOfTail]
		      shipPairNotIntersects (first, second)
		      	= isNothing $ shipBounds first `rangeIntersect` shipBounds second
