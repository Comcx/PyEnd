module Main where

import System.Environment
import PyEnd

noFileName = putStrLn "Please tell me the file name"

ioFileName i o = do
  (r, n, b) <- end <$> readFile i
  writeFile o r

soleFileName s = ioFileName s ('_':s)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> noFileName
    x:[]   -> soleFileName x
    x:y:ss -> ioFileName x y
      



