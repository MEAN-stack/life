module Main(main) where

import Graphics.Gloss
import Data.List
import System.IO

-- helper functions for 2D arrays [[Int]]
-- map, zip, and zipWith
-- 
gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap = map.map

gridZip :: [[a]] -> [[b]] -> [[(a, b)]]
gridZip = zipWith zip

gridZipWith :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
gridZipWith f = zipWith (zipWith f)

-- the rules of life
-- 
live = 1
die = 0

-- (x, y)
-- x = 0 - uninhabited (dead) cell
-- x = 1 - inhabited (live) cell
-- y = number of neighbours *including* the cell itself
-- 
rulesOfLife :: (Int, Int) -> Int
rulesOfLife (1, 3) = live
rulesOfLife (1, 4) = live
rulesOfLife (0, 3) = live
rulesOfLife (_, _) = die

-- The game takes place in a square grid of size l
--
l = 100 :: Int
l2 = l `div` 2

-- Spawn the next generation
-- map each cell to its neighbour count
-- zip with the original array
-- map the life rules over the result
-- 
-- 0 1 1      2 3 1      (0,2) (1,3) (1,1)      0 1 0
-- 1 0 0  ->  3 4 3  ->  (1,3) (0,4) (0,3)  ->  1 0 1
-- 0 1 0      2 2 1      (0,2) (1,2) (0,1)      0 0 0
-- 
generate :: [[Int]] -> [[Int]]
generate x = gridMap rulesOfLife (gridZip x u)
             where y = gridZipWith (+) x (zs:x)
                   w = transpose $ gridZipWith (+) y ((tail x)++[zs])
                   v = gridZipWith (+) w (zs:w)
                   u = transpose $ gridZipWith (+) v ((tail w)++[zs])
                   zs = replicate l 0

-- Draw the array of cells
-- First flatten the array and convert each cell value to a triple
-- (val, x, y) where (x,y) are the coordinates of the cell
-- filter out the inhabited cells
-- draw each cell as a black rectangle
-- 
drawer :: [[Int]] -> Picture
drawer x = pictures $ map d zz
           where y = zip (concat x) [0..]
                 m (x, y) = (x, fromIntegral (y `mod` l), fromIntegral (y `div` l))
                 z = map m y
                 zz = filter (\(v,x,y) -> v==1) z
                 d (_,col,row) = translate ((col-l2)*10) ((l2-row)*10) $ color black $ rectangleSolid 8 8

-- Extend a 2-D array in all directions
-- 
extend :: [[Int]] -> [[Int]]
extend = transpose . map ext . transpose . map ext

-- extend a 1-D array to left and right
-- 
ext :: [Int] -> [Int]
ext x = (replicate len1 0) ++ x ++ (replicate len2 0)
        where len = length x
              len1 = (l - len) `div` 2
              len2 = l - len - len1

-- Read the seed from a file.
-- Convert it to an array of 1s and 0s
-- Extend the array in all directions with 0s to a size of 100 x 100
-- 
-- Start an animation, drawing the cells for each generation of life
--
main :: IO ()
main = do
    handle <- openFile "seed.txt" ReadMode
    contents <- hGetContents handle
    let x = extend $ gridMap (\s -> if s=="0" then 0 else 1) $ map words $ lines contents
    simulate (InWindow "Life" (l*10, l*10) (10, 10)) white 25 x drawer (\_ dt x -> generate x)
    hClose handle
