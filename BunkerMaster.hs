-- A program to take line segments in and find intersections fast
-- Challenge from: http://thirdpartyninjas.com/blog/2008/10/07/line-segment-intersection/
-- Intersection algorithm from: http://stackoverflow.com/a/565282/786339

import Data.Maybe						-- To make Maybe handling easier
import Data.List						-- So we can sort
import System.IO						-- For file loading

------------------ Some types we'll use ------------------

data TerrainType = Nest | Impassible | Unreliable | Reliable | Bunker | Wall
					derriving (Show, Eq)

data Terrain = Terrain {
					kind :: TerrainType,
					bugs :: Bool,
					humans :: Bool
				}

data Point = Point Int Int derriving Eq

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

instance Show Map where
	show = unlines $ map (\x -> concat $ map show x)	-- Convert chars with show, join with concat, join with unlines
		
------------------ Functions to work on our types ------------------

parseTerrain :: Char -> Maybe Terrain
parseTerrain c = case c of
					"*"	-> Just $ Terrain
					"#"	-> Just $ Terrain
					"+"	-> Just $ Terrain
					"-"	-> Just $ Terrain
					"o"	-> Just $ Terrain
					"@"	-> Just $ Terrain
					_	-> Nothing

------------------ Our main function, to do the work ------------------

main = do
	putStrLn "Nothing"