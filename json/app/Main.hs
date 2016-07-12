module Main where

import           PrettyJSON
import           PutJSON
import           SimpleJSON

main :: IO ()
main = putJValue (JObject [("foo", JNumber 1), ("bar", JBool False)])
