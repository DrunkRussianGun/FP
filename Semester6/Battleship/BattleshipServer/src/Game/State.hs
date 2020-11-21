module Game.State
	( Player(..)
	, State
	, currentPlayer
	, players
	, createGame
	)
	where

import Game.Map

import Data.List
import Data.Text (Text)
import qualified Data.Text as Text

data Player = Player
	{ playerName :: Text
	, playerMap :: Map
	}
	deriving (Show, Read)

instance Eq Player where
	first == second = playerName first == playerName second

data State = State
	{ currentPlayer :: Player -- ^ Игрок, который в данный момент делает ход
	, players :: [Player]     -- ^ Все игроки, участвующие в данной игре
	}
	deriving (Show, Read)

createGame :: Player -> [Player] -> Maybe State
createGame firstPlayer players
	| (length players == length (nub players))
		&& all (== head playerMapBounds) (tail playerMapBounds)
		= Just $ State firstPlayer players
	| otherwise = Nothing
		where playerMapBounds = map (mapBounds . playerMap) players
