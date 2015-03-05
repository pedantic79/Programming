import Control.Monad
import Text.Printf

produceRGB c = [ (r,g,b) | b <- c, g <- c, r <- c ]

toHexColor :: (Integer, Integer, Integer) -> String
toHexColor (r,g,b) = printf "%02x/%02x/%02x" r g b

colors = produceRGB [0, 95, 135, 175, 215, 255]

simple = produceRGB [0, 191] ++ produceRGB [64, 255]

-- putStr $ printf "\o33[38;5;%dHello\o33[0m" 10
-- mapM_ print $ map toHexColor simple

printColors :: [(Integer, Integer, Integer)] -> Integer -> IO()
printColors colorList start = do
  printColors' colorList 0
  where
    printColors' :: [(Integer, Integer, Integer)] -> Integer -> IO()
    printColors' (c:cs) i = do
      putStr $ printf "\o33[48;5;%dm%s\o33[0m" (i + start) $ toHexColor c
      when (mod i 5 == 4) $ putStrLn ""
      printColors' cs $ i + 1
    printColors' _ _ = do
      putStrLn ""

  
