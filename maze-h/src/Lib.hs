module Lib where

import Data.Maybe (listToMaybe)
import Data.List
import Data.Function (on)
import Control.Applicative (Alternative(..), (<|>))

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
parseMaze =
 concatMap (\(y, str) -> zipWith (\x ch -> ((x, y), ch)) [0..] str) . zip [0 ..]

firstPath :: ([Path] -> Cell-> Maze -> Maybe Path)
               -> Cell -> Cell -> Maze -> Maybe Path
firstPath f p0 = f [[p0]]

_byB :: Alternative m => [Path] -> Cell -> Maze -> m Path
_byB [] _ _ = empty
_byB ([]:queue) p1 mz = _byB queue p1 mz
_byB ap@(path@(p:_):queue) p1 mz 
 | p == p1 = pure path <|> _byB queue p1 mz
 | otherwise = 
    _byB (queue ++ [x : path | x <- nextSteps p mz, all (x `notElem`) ap]) p1 mz


findPoint :: Char -> Maze -> Maybe Cell
findPoint c = fmap  fst . find (\val -> snd val == c)

solve :: [String] -> Maybe [String]
solve ls = do
  let mz = parseMaze ls
  st <- findPoint 'S' mz
  gl <- findPoint 'G' mz
  path <- firstPath _byB st gl mz
  let f c ch = if c `elem` path && ch == ' '
               then '$' else ch
  let solvedMaze = map (\(c, ch) -> (c, f c ch)) mz
  pure .  map (map snd . sortOn fst) . groupBy ((==) `on` snd . fst) . sortOn (snd . fst) $ solvedMaze
  

runSolve :: FilePath -> IO ()
runSolve fp = do
  ln <- readFile fp
  let maySolve = solve . lines $ ln
  case maySolve of
    Nothing -> putStrLn "there is no solution"
    Just solve -> mapM_ putStrLn solve
