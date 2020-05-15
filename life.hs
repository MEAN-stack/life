module Main(main) where

import Graphics.Gloss
import Data.List

gen (1,y) = if y==3 || y==4 then 1 else 0
gen (_,y) = if y==3 then 1 else 0

gen1 x = (map.map) gen (zipWith zip x u)
         where y = zipWith (zipWith (+)) x ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]:x)
               w = transpose $ zipWith (zipWith (+)) y ((tail x)++[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]])
               v = zipWith (zipWith (+)) w ([0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]:w)
               u = transpose $ zipWith (zipWith (+)) v ((tail w)++[[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]])

x = [
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,1,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,1,1,0,0,0],
  [0,0,0,0,0,0,1,1,0,1,0,0,0,0,0],
  [0,0,0,0,0,1,1,1,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,1,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
  [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]

m (x, y) = (x, fromIntegral (y `mod` 15), fromIntegral (y `div` 15))

drawer x = pictures $ map d z
           where y = zip (concat x) [0..]
                 z = map m y
                 d (0,col,row) = translate ((col-15)*15) ((15-row)*15) $ color white $ rectangleSolid 26 26
                 d (_,col,row) = translate ((col-15)*15) ((15-row)*15) $ color black $ rectangleSolid 26 26

main = simulate (InWindow "Life" (500, 500) (10, 10)) white 10 x drawer (\_ dt x -> gen1 x)
