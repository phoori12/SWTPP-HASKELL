import Test.QuickCheck
import Lib (pal)prop_reverseInvariant :: String -> Bool
prop_reverseInvariant text = pal text == pal (reverse text)

main :: IO ()
main = do
  quickCheck prop_reverseInvariant