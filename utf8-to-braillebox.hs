import Data.Char
import Data.Bits

utf8ToBrailleBox :: String -> String
utf8ToBrailleBox = concatMap utf8CharToBrailleBox

utf8CharToBrailleBox :: Char -> String
utf8CharToBrailleBox c =
 case n > 10240 && n < 10495 of
  True -> "\\braillebox{"++dots++"}"
  False -> [c]
 where
  n = ord c
  v = n-10240
  dots = concatMap up [1..8]
  up :: Int -> String
  up x =
   case isUp x of
    True -> show x
    False -> " "
  isUp x = testBit v (x-1)
