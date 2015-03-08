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

printColors :: [(Integer, Integer, Integer)] -> Integer -> IO ()
printColors colorList start = printColors' colorList 1
  where
    printColors' (c:cs) i =
      let color = start + i in
      do
        printNum color
        printVal color $ toHexColor c
        when (mod i 6 == 0) $ putStrLn ""
        printColors' cs $ i + 1
    printColors' _ _ = return ()
    printNum num =
      putStr $ printf "\o33[1;38;5;%dm%3s: \o33[0m" num $ show num
    printVal color value =
      putStr $ printf "\o33[38;5;%dm%s\o33[0m  " color value

main = do
--  printColors simple 0
--  putStrLn ""
  printColors colors 15
  printColors grey 231
