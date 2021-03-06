module Main where
import Control.Monad (when)
import Text.Printf (printf)

data Color = Color { r :: Int, g :: Int, b :: Int }

instance Show (Color) where
  show (Color r g b) = printf "%02x/%02x/%02x" r g b

colors :: [Color]
colors = produceRGB [0, 95, 135, 175, 215, 255]
  where produceRGB l = [ Color r g b | r <- l, g <- l, b <- l ]

simple :: [Color]
simple = produceRGB [0, 191] ++ produceRGB [64, 255]
  where produceRGB l = [ Color r g b | b <- l, g <- l, r <- l ]

grey :: [Color]
grey = produceRGB [8, 18..240]
  where produceRGB l = [ Color c c c | c <- l ]

colorsToData :: Int -> [Color] -> [(Int, String, Bool)]
colorsToData start = colorsToData' 0
  where
    colorsToData' i (c:cs) = datum : colorsToData' (i + 1) cs
      where datum = (start + i, show c, newLine)
            newLine = i `mod` 6 == 5
    colorsToData' _ _ = []

printColor :: (Int, String, Bool) -> IO ()
printColor (color, hex, newLine) = do
  putStr $ printf "\o33[1;38;5;%dm%3s: \o33[0m" color $ show color
  putStr $ printf "\o33[38;5;%dm%s\o33[0m  " color hex
  when newLine $ putStrLn ""

printColors :: Int -> [Color] -> IO ()
printColors = (mapM_ printColor .) . colorsToData

main :: IO ()
main = do
--  printColors 0 simple
--  putStrLn ""
  printColors 16 colors
  printColors 232 grey
