module PutJSON where

import           Data.List  (intercalate)
import           SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s) = show s
renderJValue (JNumber a) = show a
renderJValue (JBool True) = "true"
renderJValue (JBool False) = "false"
renderJValue JNull = "null"
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where
        pairs :: [(String, JValue)] -> String
        pairs [] = ""
        pairs ps = intercalate ", " $ map renderPair ps

        renderPair :: (String, JValue) -> String
        renderPair (name, value) = show name ++ ":" ++ renderJValue value

renderJValue (JArray a) = "[" ++ values a ++ "]"
    where
        values :: [JValue] -> String
        values [] = ""
        values vs = intercalate ", " $ map renderJValue vs

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
