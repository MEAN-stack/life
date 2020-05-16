import Graphics.Gloss
import Data.List
import System.IO

gridMap = map.map
gridZip = zipWith zip
gzwp = zipWith (zipWith (+))
live = 1
die = 0

rulesOfLife (1, y) = if y==3 || y==4 then live else die
rulesOfLife (0, 3)    = live
rulesOfLife (_, _)    = die

l = 100 :: Int
l2 = 50

generate x = gridMap rulesOfLife (gridZip x u)
             where y = gzwp x (zs:x)
                   w = transpose $ gzwp y ((tail x)++[zs])
                   v = gzwp w (zs:w)
                   u = transpose $ gzwp v ((tail w)++[zs])
                   zs = replicate l 0

m (x, y) = (x, fromIntegral (y `mod` l), fromIntegral (y `div` l))

drawer x = pictures $ map d z
           where y = zip (concat x) [0..]
                 z = map m y
                 d (0,col,row) = translate ((col-l2)*10) ((l2-row)*10) $ color white $ rectangleSolid 8 8
                 d (_,col,row) = translate ((col-l2)*10) ((l2-row)*10) $ color black $ rectangleSolid 8 8

extend :: [[Int]] -> [[Int]]
extend = transpose . map ext . transpose . map ext

ext :: [Int] -> [Int]
ext x = (replicate len1 0) ++ x ++ (replicate len2 0)
        where len = length x
              len1 = (l - len) `div` 2
              len2 = l - len - len1

main = do
    handle <- openFile "seed.txt" ReadMode
    contents <- hGetContents handle
    let x = extend $ gridMap (\s -> if s=="0" then 0 else 1) $ map words $ lines contents
    simulate (InWindow "Life" (l*10, l*10) (10, 10)) white 25 x drawer (\_ dt x -> generate x)
    hClose handle
