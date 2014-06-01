-- A program to take figure out how to protect humans for horrible termites
-- Challenge from: http://www.reddit.com/r/dailyprogrammer/comments/26oop1/5282014_challenge_164_intermediate_part_3_protect/

import qualified Data.Set as Set

------------------ Some types we'll use ------------------

data TerrainType = Nest | Impassible | Unreliable | Reliable | Bunker | Wall deriving Eq

data Terrain = Terrain {
					kind :: TerrainType,
					bugs :: Bool,
					humans :: Bool
				}

data MapSize = MapSize Int Int

type Point = (Int, Int)

type Map = [[Terrain]]

------------------ Instances so we can show things easily ------------------

instance Show TerrainType where
	show t = case t of
				Nest		-> "*"		-- Put the character back
				Impassible	-> "#"
				Unreliable	-> "+"
				Reliable	-> "-"
				Bunker		-> "o"
				Wall		-> "@"

instance Show Terrain where
	show = show . kind					-- Just display the terrain type, ignore the two bools
		
------------------ Functions to work on our types ------------------

-- Turn our map into a displayable string
showMap :: Map -> String
showMap m = unlines $ map (\x -> concat $ map show x) m	-- Convert chars with show, join with concat, join with unlines

-- Determine map size so we don't have to pass it around
findMapSize :: Map -> MapSize
findMapSize m@(r:_) = MapSize (length r) (length m)

-- Simple bounds check function
validPoint :: MapSize -> Point -> Bool
validPoint (MapSize w h) (x, y)
	| x <= 0 || y <= 0	= False
	| x > w || y > h	= False
	| otherwise			= True

parseTerrain :: Char -> Terrain
parseTerrain c = case c of
					'*'	-> Terrain Nest True False
					'#'	-> Terrain Impassible False False
					'+'	-> Terrain Unreliable False False
					'-'	-> Terrain Reliable False False
					'o'	-> Terrain Bunker False True
					'@'	-> Terrain Wall True True

-- Given a function to test a terrain type and a row and it's Y index generate the coords of matching squares
findLocationsInRow :: (TerrainType -> Bool) -> [Terrain] -> Int -> [Point]
findLocationsInRow f r i = [ (x + 1, i) | x <- [0,1..(length r)], f . kind $ r !! x ]

-- Find all matching locations in a map. Use zipWith to generate a list of lists of points, concat to join it together
findLocations :: (TerrainType -> Bool) -> Map -> [Point]
findLocations f m = concat $ zipWith (findLocationsInRow f) m [1,2..]

-- Get a given piece of terrain
findTerrain :: Map -> Point -> Terrain
findTerrain m (x, y) = m !! (y - 1) !! (x - 1)

-- Filter a list of updates to skip places the entity has already visited
filterVisited :: (Terrain -> Bool) -> Map -> [Point] -> [Point]
filterVisited f m [] = []
filterVisited f m (p:ps)
	| f $ findTerrain m p	= p : filterVisited f m ps
	| otherwise				= filterVisited f m ps

-- Figure out if we can place a wall on a square
canWall (Terrain Reliable _ _)	= True
canWall _						= False

-- Figure out if the bugs can walk on a square
bugCrossable Unreliable	= True
bugCrossable Reliable	= True
bugCrossable _			= False

-- Figure out if the humans can walk on a square
humanCrossable	Reliable	= True
humanCrossable _			= False

-- Convenience functions to find locations
bugStart :: Map -> [Point]
bugStart = findLocations ((==) Nest)

humanStart :: Map -> [Point]
humanStart = findLocations ((==) Bunker)

wallLocations :: Map -> [Point]
wallLocations = findLocations ((==) Wall)

-- Find the neighboring cells
neighbors :: Map -> Point -> [Point]
neighbors m (x, y) = [ (x + i, y + j) | i <- [-1,1], j <- [-1,1], validPoint size (x + i, y + j) ]
	where
		size = findMapSize m

-- Generate the list of squares that we need to process next round
findPointsToProcess :: Map -> [Point] -> (Terrain -> Bool) -> (Terrain -> Bool) -> Set.Set Point
findPointsToProcess _ [] _ _ = Set.empty							-- Nothing to check? Nothing done
findPointsToProcess m ps walkable visited =
		Set.filter checkPoint allNeighborsSet						-- Keep only things that are walkable and not visited
	where
		neighborLists	= map (neighbors m) ps							-- Generate neighbors for each point
		allNeighborsSet	= Set.fromList $ concat neighborLists			-- Collapse that		
		checkPoint p	= (walkable terrain) && (not . visited) terrain	-- Function to test locations
			where 
				terrain = findTerrain m p

-- Update terrain where a bug has touched it, return the update and if we need to keep going
bugify :: Terrain -> (Terrain, Bool)
bugify t@(Terrain kind buggy humany)
	| buggy					= (t, False)							-- Already been here
	| crossable && humany	= (t{kind = Wall, bugs = True}, False)	-- Humans have been here and it's good, wall it up
	| crossable 			= (t{bugs = True}, True)				-- No humans but we can cross, so do it
	| otherwise				= (t, False)							-- Can't cross, don't bother
	where
		crossable = bugCrossable kind

-- Update terrain where a bug has touched it, return the update and if we need to keep going
humanize :: Terrain -> (Terrain, Bool)
humanize t@(Terrain kind buggy humany)
	| humany				= (t, False)								-- Already been here
	| crossable && buggy	= (t{kind = Wall, humans = True}, False)	-- Bugs have been here and it's good, wall it up
	| crossable 			= (t{humans = True}, True)					-- No bugs but we can cross, so do it
	| otherwise				= (t, False)								-- Can't cross, don't bother
	where
		crossable = humanCrossable kind

-- Update points in the set with the given function collecting coords of updated squares that need continued processing
updateTerrainRow :: (Terrain -> (Terrain, Bool)) -> Set.Set Point -> [Terrain] -> Int -> ([Terrain], [Point])
updateTerrainRow f toUpdate t y = foldl helper ([], [])	thatBackwards	-- We'll fold over the row to do the updates
	where
		withXPositions	= zip t [1,2..]									-- Convert terrain list to [(Terrain, Int)]
		thatBackwards	= reverse withXPositions						-- Reverse that because our foldl will reverse it again
		helper (ts, ps) (t, x)
			| needsUpdate && keepGoing	= (updated:ts, point:ps)		-- Update terrain, collect point for next update
			| needsUpdate				= (updated:ts, ps)				-- Update terrain, don't bother collecting point
			| otherwise					= (ts, ps)						-- No changes
				where
					point = (x, y)
					needsUpdate = Set.member point toUpdate
					(updated, keepGoing) = f t
		
-- Update all the terrain with the given update function, collect the coords of squares that we need to keep working with
updateTerrain :: (Terrain -> (Terrain, Bool)) -> Set.Set Point -> Map -> (Map, [Point])
updateTerrain f toUpdate map
	| Set.null toUpdate	= (map, [])										-- Nothing to update? No changes
	| otherwise			= foldl helper ([], []) thatBackwards			-- We'll fold over the rows to do the updates
	where
		withRowNumbers			= zip map [1,2..]						-- Convert the map to [([Terrain], Int)]
		thatBackwards			= reverse withRowNumbers				-- Reverse that because our foldl will reverse it again
		helper (rs, ps) (r, y) = (updatedRow:rs, additionalPoints ++ ps)
			where
				(updatedRow, additionalPoints) = updateTerrainRow f toUpdate r y

------------------ Our main function, to do the work ------------------

main = do
	putStrLn "Nothing"