module Main(main) where

import Graphics.Gloss
import Data.List

gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap = map.map

gridZip :: [[a]] -> [[b]] -> [[(a, b)]]
gridZip = zipWith zip 

-- gridZipWithPlus
gzwp :: Num c => [[c]] -> [[c]] -> [[c]]
gzwp = zipWith (zipWith (+))

live = 1
die = 0
zs = replicate 15 0

-- Conway's rules
rulesOfLife (1, y) = if y==3 || y==4 then live else die
rulesOfLife (_, y)    = if y==3 then live else die

generate x = gridMap rulesOfLife (gridZip x u)
             where y = gzwp x (zs:x)
                   w = transpose $ gzwp y ((tail x)++[zs])
                   v = gzwp w (zs:w)
                   u = transpose $ gzwp v ((tail w)++[zs])

-- "pi" seed
x = [zs,zs,zs,zs,zs,zs,[0,0,0,0,0,0,1,1,1,0,0,0,0,0,0],[0,0,0,0,0,0,1,0,1,0,0,0,0,0,0],[0,0,0,0,0,0,1,0,1,0,0,0,0,0,0],zs,zs,zs,zs,zs,zs]

m (x, y) = (x, fromIntegral (y `mod` 15), fromIntegral (y `div` 15))

drawer x = pictures $ map d z
           where y = zip (concat x) [0..]
                 z = map m y
                 d (0,col,row) = translate ((col-15)*15) ((15-row)*15) $ color white $ rectangleSolid 26 26
                 d (_,col,row) = translate ((col-15)*15) ((15-row)*15) $ color black $ rectangleSolid 26 26

main = simulate (InWindow "Life" (500, 500) (10, 10)) white 10 x drawer (\_ dt x -> generate x)
