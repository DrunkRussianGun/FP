module Game.Moves
	( Move(..)
	, ShootMove
	, shootingPlayer
	, shootedPlayer
	, shootedAt
	, createShootMove
	) where

import Game.Map
import Game.State

class Move a where
	apply :: a -> State -> State

data ShootMove = ShootMove
	{ shootingPlayer :: Player
	, shootedPlayer :: Player
	, shootedAt :: Cell
	}

createShootMove :: Player -> Player -> Cell -> Maybe ShootMove
createShootMove shootingPlayer shootedPlayer shootedAt
	| (mapBounds . playerMap) shootedPlayer `rangeContains` shootedAt
		= Just $ ShootMove shootingPlayer shootedPlayer shootedAt
	| otherwise = Nothing

instance Move ShootMove where
	apply move oldState = undefined
