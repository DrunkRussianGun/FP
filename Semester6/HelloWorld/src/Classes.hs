module Classes (module Classes) where

-- Аналог интерфейса
class Equalable a where
	(==) :: a -> a -> Bool
	(!=) :: a -> a -> Bool

data StupidValue = Value Int

-- Аналог класса, реализующего интерфейс
instance Equalable StupidValue where
	(==) (Value left) (Value right) = left /= right
	(!=) (Value left) (Value right) = left Prelude.== right
	-- По умолчанию неравенство реализовано так:
	-- (!=) left right = not $ left == right
	-- Поэтому можно не писать свою реализацию, тогда будет использоваться реализация по умолчанию

data Tree a
	= Leaf a
	| Node a [Tree a]
	deriving (Show)

-- Если тип a реализует Eq, то Tree a реализует Eq
instance Eq a => Eq (Tree a) where
	(==) leftTree rightTree = case leftTree of
		Leaf leftLeaf -> case rightTree of
			Leaf rightLeaf -> leftLeaf Prelude.== rightLeaf
			Node _ _ -> False
		Node leftLeaf leftRest -> case rightTree of
			Leaf _ -> False
			Node rightLeaf rightRest -> leftLeaf Prelude.== rightLeaf && leftRest Prelude.== rightRest
	Leaf leftLeaf /= Leaf rightLeaf = leftLeaf /= rightLeaf
	Leaf _ /= Node _ _ = False
	Node _ _ /= Leaf _ = False
	Node leftLeaf leftRest /= Node rightLeaf rightRest = leftLeaf /= rightLeaf && leftRest /= rightRest

data IntOrChar
	= Int Int
	| Char Char

instance Eq IntOrChar where
	Int left == Int right = left Prelude.== right
	Char left == Char right = left Prelude.== right
	_ == _ = False
instance Ord IntOrChar where
	compare (Int left) (Int right) = Prelude.compare left right
	compare (Int _) (Char _) = LT
	compare (Char _) (Int _) = GT
	compare (Char left) (Char right) = Prelude.compare left right
