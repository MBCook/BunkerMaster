-- A program to take line segments in and find intersections fast
-- Challenge from: http://thirdpartyninjas.com/blog/2008/10/07/line-segment-intersection/
-- Intersection algorithm from: http://stackoverflow.com/a/565282/786339

import Data.Maybe						-- To make Maybe handling easier
import Data.List						-- So we can sort
import System.IO						-- For file loading

------------------ Some types we'll use ------------------

data TerrainType = Nest | Impassible | Unreliable | Reliable | Bunker | Wall deriving Eq

data Terrain = Terrain {
					kind :: TerrainType,
					bugs :: Bool,
					humans :: Bool
				}

data Point = Point Int Int deriving Eq

data MapSize = MapSize Int Int

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

showMap :: Map -> String
showMap m = unlines $ map (\x -> concat $ map show x) m	-- Convert chars with show, join with concat, join with unlines

validPoint :: MapSize -> Point -> Bool	-- Simple bounds check function
validPoint (MapSize w h) (Point x y)
	| x <= 0 || y <= 0	= False
	| x > w || y > h	= False
	| otherwise			= True

parseTerrain :: Char -> Maybe Terrain
parseTerrain c = case c of
					'*'	-> Just $ Terrain Nest True False
					'#'	-> Just $ Terrain Impassible False False
					'+'	-> Just $ Terrain Unreliable False False
					'-'	-> Just $ Terrain Reliable False False
					'o'	-> Just $ Terrain Bunker False True
					'@'	-> Just $ Terrain Wall True True
					_	-> Nothing

-- Given a function to test a terrain type and a row and it's Y index generate the coords of matching squares
findLocationsInRow :: (TerrainType -> Bool) -> [Terrain] -> Int -> [Point]
findLocationsInRow f [] _	= []
findLocationsInRow f r i	= [ Point (x + 1) i | x <- [0,1..(length r)], f . kind $ r !! x ]

-- Find all matching locations in a map. Use zipWith to generate a list of lists of points, concat to join it together
findLocations :: (TerrainType -> Bool) -> Map -> [Point]
findLocations f m = concat $ zipWith (findLocationsInRow f) m [1,2..]

-- Figure out if we can place a wall on a square
canWall (Terrain Reliable _ _)	= True
canWall _						= False

-- Figure out if the bugs can walk on a square
bugsCross (Terrain Unreliable _ _)	= True
bugsCross (Terrain Reliable _ _)	= True
bugsCross _							= False

-- Figure out if the humans can walk on a square
humansCross	(Terrain Reliable _ _)	= True
humansCross _						= False

-- Convenience functions to start us out
bugStart :: Map -> [Point]
bugStart = findLocations ((==) Nest)

humanStart :: Map -> [Point]
humanStart = findLocations ((==) Bunker)

------------------ Our main function, to do the work ------------------

main = do
	putStrLn "Nothing"