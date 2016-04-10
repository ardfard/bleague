{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Protolude hiding (get)
import           Web.Scotty

main :: IO ()
main = scotty 5000 $ do
  get "/" $ file "./web/index.html"

