module PrettyJSON where

import           SimpleJSON

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

-- lessons learned here is that undefined
-- can be used to seam together types without
-- implementing acutual functionality.

-- you start implementing at the entry point, in this
-- case string, and then just make the type checker
-- happy with undefined. You can then implement functions from the leaves
-- until all of the undefineds are gone, testing along the way.

-- Implementation plan string -> enclose -> oneChar.

data Doc =
    ToBeDefined deriving (Show)

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose left right doc = char left <> doc <> char right

char :: Char -> Doc
char = undefined

(<>) :: Doc -> Doc -> Doc
(<>) = undefined

hcat :: [Doc] -> Doc
hcat = undefined

oneChar :: Char -> Doc
oneChar c =
    case lookup c simpleEscapes of
        Just r -> text r
        Nothing | mustEscape c -> hexEscape c
                | otherwise -> char c
    where
        mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes =
    zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    -- String is just [Char] so the second element
    -- of the tuple is really a string.
    -- For some reason the \\ Char is special and
    -- can have two charaters in the Char.
    where ch a b = (a, ['\\', b])

hexEscape :: Char -> Doc
hexEscape = undefined
