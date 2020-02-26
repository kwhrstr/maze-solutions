module Lib where

import Data.Maybe (listToMaybe)
import Control.Monad

type Cell = (Int, Int)
type Maze = [(Cell, Char)]
type Path = [Cell]

canMove :: Maze -> Maze
canMove  = filter (\s -> snd s /= '*')

nextSteps :: Cell -> Maze -> Path
nextSteps (x, y) mz =
  let moveAble = canMove mz
  in filter (`elem` map fst moveAble)
      [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
  
parseMaze :: [String] -> Maze
parseMaze ls =
  [ ((x, y), ls !! y !! x)
  | y <- [0 .. length ls - 1]
  , x <- [0 .. length (ls !! y) - 1]
  ]


firstPath :: ([Path] -> Cell-> Maze -> Maybe Path)
               -> Cell -> Cell -> Maze -> Maybe Path
firstPath f p0 = f [[p0]]

_byB :: MonadPlus m => [Path] -> Cell -> Maze -> m Path
_byB [] _ _ = mzero
_byB ap@(path:queue) p1 mz
  | head path == p1 = return (reverse path) `mplus` _byB  queue p1 mz
  | otherwise =
    _byB (queue ++ [x : path | x <- nextSteps (head path) mz, all (x `notElem`) ap]) p1 mz


findPoints :: Char -> Maze -> [Cell]
findPoints c = map  fst . filter (\val -> snd val == c)

solve :: [String] -> Maybe [String]
solve ls = do
  let mz = parseMaze ls
  st <- listToMaybe $ findPoints 'S' mz
  gl <- listToMaybe $ findPoints 'G' mz
  path <- firstPath _byB st gl mz
  let ans =
        [ [ ch
          | x <- [0 .. length (ls !! y) - 1]
          , let ch =
                  if (x, y) `elem` path && ls !! y !! x == ' '
                  then '$'
                  else ls !! y !! x
          ]
        | y <- [0 .. length ls - 1]
        ]
  return ans

runSolve :: FilePath -> IO ()
runSolve fp = do
  ln <- readFile fp
  let maySolve = solve . lines $ ln
  case maySolve of
    Nothing -> putStrLn "there is no solution"
    Just solve -> mapM_ putStrLn solve
