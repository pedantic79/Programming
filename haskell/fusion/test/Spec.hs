import Test.Tasty
import Test.Tasty.QuickCheck
import Lib

prop_even :: [Integer] -> Bool
prop_even xs = filt even xs == filter even xs

prop_none :: [Integer] -> Bool
prop_none xs = null . filt even . filt odd $ xs

prop_even2 :: [Integer] -> Property
prop_even2 xs = collect (length xs) $
  filt even xs == filter even xs

main = do
  putStrLn("")

  defaultMain $ testGroup "Tests"
    [ testProperty "+ even" $ prop_even
    , testProperty "+ none" $ prop_even
    , testProperty "+foo" $ prop_even2
    ]
