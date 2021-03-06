module Lists (module Lists) where

-- data [a] = [] | a : [a]

-- Объединение двух списков
-- Стандартная функция (++)
-- append [1, 2] [3, 4, 5] == [1, 2, 3, 4, 5]
append :: [a] -> [a] -> [a]
append leftList rightList = case leftList of
	[] -> rightList
	(leftFirst : leftRest) -> leftFirst : append leftRest rightList

-- Список без последнего элемента
-- Стандартная функция init
-- withoutLast [1, 2, 3] == [1, 2]
withoutLast :: [a] -> [a]
withoutLast list = case list of
	[] -> error "Невозможно убрать последний элемент из пустого списка"
	[element] -> []
	(first : rest) -> first : withoutLast rest

-- Группировка одинаковых элементов
-- Стандартная функция Data.List.group
-- group [1, 1, 2, 3, 3, 3] == [[1, 1], [2], [3, 3, 3]]
group :: Eq a => [a] -> [[a]]
group list = case list of
	[] -> []
	[element] -> [[element]]
	(first : second : rest) -> case first == second of
		False -> [first] : group (second : rest)
		True  ->
			let groupingOfSecond : restOfGroupings = group (second : rest)
			in (first : groupingOfSecond) : restOfGroupings

-- Вставка элемента между остальными элементами в списке 
-- Стандартная функция Data.List.intersperse
-- intersperse 1 [2, 3, 4, 5] == [2, 1, 3, 1, 4, 1, 5]
intersperse :: a -> [a] -> [a]
intersperse elementToInsert list = case list of
	[] -> []
	[element] -> [element]
	(first : second : rest) -> first : elementToInsert : intersperse elementToInsert (second : rest)

-- Транспонирование
-- Стандартная функция Data.List.transpose
-- transpose [[1, 2, 3], [4, 5, 6]] == [[1, 4], [2, 5], [3, 6]]
transpose :: [[a]] -> [[a]]
transpose rows = case rows of
	[] -> []
	(firstRow : restOfRows) -> putRowOnTop firstRow $ transpose restOfRows where
		putRowOnTop row columns = case columns of
			[] -> case row of
				[] -> []
				(firstFromRow : restOfRow) -> [firstFromRow] : putRowOnTop restOfRow []
			(firstColumn : restOfColumns) ->
				let (firstFromRow : restOfRow) = row
				in (firstFromRow : firstColumn) : putRowOnTop restOfRow restOfColumns

-- map [1, 2, 3, 4] $ (+) 1 == [2, 3, 4, 5]
map :: [a] -> (a -> b) -> [b]
map list mapFunction = case list of
	[] -> []
	(first : rest) -> mapFunction first : Lists.map rest mapFunction

filter :: [a] -> (a -> Bool) -> [a]
filter list predicate = case list of
	[] -> []
	(first : rest) ->
		let filteredList = Lists.filter rest predicate
		in case predicate first of
			False -> filteredList
			True  -> first : filteredList

foldl :: [a] -> b -> (b -> a -> b) -> b
foldl list accumulator foldFunction = case list of
	[] -> accumulator
	(first : rest) -> Lists.foldl rest (accumulator `foldFunction` first) foldFunction

foldr :: [a] -> b -> (a -> b -> b) -> b
foldr list accumulator foldFunction = case list of
	[] -> accumulator
	(first : rest) -> first `foldFunction` Lists.foldr rest accumulator foldFunction
