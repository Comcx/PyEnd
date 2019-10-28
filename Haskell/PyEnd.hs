module PyEnd where

import Data.Char (isSpace)

type Line = String
type Code = String


xor :: Bool -> Bool -> Bool
xor True a = not a
xor False a = a

toLine :: String -> Line
toLine s = s ++ "\n"

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

wash :: Line -> Line
wash = toLine . trim

tab :: Int -> Line -> Line
tab n s = take (n * 2) (repeat ' ') ++ s

endByColon :: Line -> Bool
endByColon "" = False
endByColon s  = case trim s of
  ""        -> False
  otherwise -> last (trim s) == ':'

startByEnd :: Line -> Bool
startByEnd "" = False
startByEnd s  = case words s of
  []   -> False
  x:xs -> x == "#end"

startInline :: Line -> Bool
startInline s = case words s of
  []   -> False
  x:xs -> x == "#line"


process :: (Code, Int, Bool) -> Line -> (Code, Int, Bool)
process (s, n, b) e =
  ( (s ++ (tab n $ toLine (trim e ++ if b then "\\" else "")))
  , n + i
  , if startByEnd e then False else b || startInline e)
  where
    i = if endByColon e then 1
        else if startByEnd e && not b then -1
          else 0

pretendInline :: Line -> Line
pretendInline s = s ++ "\\"

-- Suppose we have the lines function
end :: Code -> (Code, Int, Bool)
end s = foldl process ("", 0, False) (lines s)






