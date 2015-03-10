module Main where
import Control.Monad (when)
import Text.Printf (printf)

toHexColor :: (Int, Int, Int) -> String
toHexColor (r,g,b) = printf "%02x/%02x/%02x" r g b

colors :: [(Int, Int, Int)]
colors = produceRGB [0, 95, 135, 175, 215, 255]
  where produceRGB l = [ (r,g,b) | r <- l, g <- l, b <- l ]

simple :: [(Int, Int, Int)]
simple = produceRGB [0, 191] ++ produceRGB [64, 255]
  where produceRGB l = [ (r,g,b) | b <- l, g <- l, r <- l ]

grey :: [(Int, Int, Int)]
grey = produceRGB [8, 18..240]
  where produceRGB l = [ (c,c,c) | c <- l ]

colorsToData :: [(Int, Int, Int)] -> Int -> [(Int, String, Bool)]
colorsToData colorList start = colorsToData' colorList 0
  where
    colorsToData' (c:cs) i = datum : colorsToData' cs (i + 1)
      where datum = (start + i, hexColor, newLine)
            hexColor = toHexColor c
            newLine = i `mod` 6 == 5
    colorsToData' _ _ = []

printColor :: (Int, String, Bool) -> IO ()
printColor (color, hex, newLine) = do
  putStr $ printf "\o33[1;38;5;%dm%3s: \o33[0m" color $ show color
  putStr $ printf "\o33[38;5;%dm%s\o33[0m  " color hex
  when newLine $ putStrLn ""

printColors :: [(Int, Int, Int)] -> Int -> IO ()
printColors colorList start = mapM_ printColor $ colorsToData colorList start

main :: IO ()
main = do
--  printColors simple 0
--  putStrLn ""
  printColors colors 16
  printColors grey 232
