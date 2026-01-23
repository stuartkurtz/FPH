-- Solve a maze.
--
-- We read a maze from stdin. The maze must be rectangular
-- containining only the following characters:
--   '#' -- indicates a hedge (unaccessible) position
--   ' ' -- indicates grass (accessible)
--   '+' -- indicates a starting point
--   '-' -- indicates an ending point
--
-- If there is a path in the maze from a start position
-- to an end position, we will print the maze to stdout
-- with the character '.' indicating a non-terminal
-- node on a shortest path. If there is no path, we
-- exit with an error.

module Main where

import Data.Array ( Array, (!), (//) )
import qualified Data.Array as A
import Data.Ix ( inRange )
import Data.Set ( Set )
import qualified Data.Set as S

type Position = (Int,Int) -- (row,col) space, with (0,0) at the upper left.
type Maze = Array Position Char

readMaze :: String -> Maze
readMaze input =
  let assocs = [ ((row,col),c)
               | (row,r) <- zip [0..] (lines input)
               , (col,c) <- zip [0..] r
               ]
      keys = map fst assocs
  in A.array (minimum keys, maximum keys) assocs

showMaze :: Maze -> String
showMaze maze =
    let ((startRow,startCol), (endRow,endCol)) = A.bounds maze
        row r = [ maze ! (r,c) | c <- [startCol .. endCol] ]
    in unlines [ row r | r <- [startRow .. endRow]]

-- Find all the positions in the maze which contain a given character.

findInMaze :: Maze -> Char -> Set Position
findInMaze maze ch = S.fromList [ pos | (pos,c) <- A.assocs maze, c == ch ]

-- Create a new maze with positions in a path marked by a period. Note
-- that we do *not* mark any start or end positions that may appear.

markPath :: [Position] -> Maze -> Maze
markPath ps maze = maze // [ (p,'.') | p <- ps, maze ! p == ' ']

-- Find a path from a start position to an end position
-- via breadth first search.
-- 
-- The top-level call to go creates a list of values
-- [posns_n .. posns_0] where posns_i consists of the set of
-- points in the maze at distance i from a start node, and
-- posns_n is the only set to contain an end node.
-- 
-- The extractPath function returns a path from a start node
-- to an end node, by calling backtrace with a end node, and
-- reversing the result.
--
-- The backtrace funciton takes a position and a 
-- [Set Position] as constructed by go, and returns
-- a path from the position to a start node.

findPath :: Maze -> [Position]
findPath maze = extractPath $ go start S.empty [] where
  start = findInMaze maze '+'
  end   = findInMaze maze '-'
  go frontier visited acc
    | S.null frontier = error "no solution"
    | S.null (frontier `S.intersection` end) =
        let visited' = frontier `S.union` visited
            frontier' = S.unions (S.map (neighbors maze) frontier) S.\\ visited'
        in go frontier' visited' (frontier : acc)
    | otherwise = frontier : acc
  extractPath [] = error "vacuous solution!?"
  extractPath (l:ls) = 
    let goal = S.findMin $ l `S.intersection` end
    in reverse $ goal : backtrack goal ls
  backtrack _ [] = error "logic error"
  backtrack pos [_] = [S.findMin $ neighbors maze pos `S.intersection` start]
  backtrack pos (a:as) = 
    let newPos = S.findMin $ neighbors maze pos `S.intersection` a
    in newPos : backtrack newPos as

-- Find the neighbors of a position in a maze.

neighbors :: Maze -> Position -> Set Position
neighbors maze (row,col) = S.fromList 
  [ pos 
  | pos <- [(row-1,col),(row,col-1),(row,col+1),(row+1,col)]
  , inRange (A.bounds maze) pos
  , maze ! pos /= '#'
  ]

-- The main act...

main :: IO ()
main = do
  input <- getContents
  let maze = readMaze input
  putStr . showMaze . markPath (findPath maze) $ maze
