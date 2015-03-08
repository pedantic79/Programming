module Main where
import Control.Monad (when)
import Text.Printf (printf)

toHexColor :: (Integer, Integer, Integer) -> String
toHexColor (r,g,b) = printf "%02x/%02x/%02x" r g b

colors = produceRGB [0, 95, 135, 175, 215, 255]
  where produceRGB l = [ (r,g,b) | r <- l, g <- l, b <- l ]

simple = produceRGB [0, 191] ++ produceRGB [64, 255]
  where produceRGB l = [ (r,g,b) | b <- l, g <- l, r <- l ]

grey = produceRGB [8, 18..240]
  where produceRGB l = [ (c,c,c) | c <- l ]

colorsToData colorList start = colorsToData' colorList 0 []
  where
    colorsToData' (c:cs) i acc =
      colorsToData' cs (i + 1) (datum : acc)
      where datum = (start + i, hexColor, newLine)
            hexColor = (toHexColor c)
            newLine = i `mod` 6 == 5
    colorsToData' _ _ acc = reverse acc

printColor (color, hex, newLine) = do
  putStr $ printf "\o33[1;38;5;%dm%3s: \o33[0m" color $ show color
  putStr $ printf "\o33[38;5;%dm%s\o33[0m  " color hex
  when newLine $ putStrLn ""

printColors :: [(Integer, Integer, Integer)] -> Integer -> IO ()
printColors colorList start = mapM_ printColor $ colorsToData colorList start

main = do
--  printColors simple 0
--  putStrLn ""
  printColors colors 16
  printColors grey 232
