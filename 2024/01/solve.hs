-- main = putStrLn "Hello, World!"

module MyLib (someFunc) where

import System.Directory

someFunc :: IO ()
someFunc = do
  contents <- listDirectory "."
  print contents

main = someFunc