module Main where

import ApiType(app)
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  run 8081 app
