module Main (main) where

import Docker.Compact (createTarball)

main :: IO ()
main = do
  putStrLn "[sentinel] running build"
  -- createTarball
