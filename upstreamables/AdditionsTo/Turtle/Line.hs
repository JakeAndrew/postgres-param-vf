{-# LANGUAGE OverloadedStrings #-}

module AdditionsTo.Turtle.Line
    ( lineToFilePath
    , unsafeFilePathToLine
    , filePathToLines
    , unsafeHostnameToLine, hostnameToLines
    , isLine
    , textToLines'
    , stringToLines
    , unsafeStringToLine
    , singleQuoteLine
    , doubleQuoteLine
    , intercalateLines
    , unsafeShowLine
    ) where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Turtle
import Data.ByteString (ByteString) -- "Hostnames" are ByteStrings.
import Data.Text.Encoding (decodeUtf8) -- Used to convert Hostnames to Lines.
import Data.List.NonEmpty (toList)
import Data.String.Combinators (quotes, doubleQuotes)
import Flow
import qualified Data.Text as Txt (pack, isInfixOf, intercalate)

import AdditionsTo.Filesystem.Path.CurrentOS

lineToFilePath :: Line -> FilePath
lineToFilePath = lineToText .> fromText

-- | Converts a Turtle.FilePath into a Line. Note the "unsafe":
-- At time of writing, Turtle.FilePaths may contain newlines. If such a path
-- was given to this function, it'd throw a "Newline Forbidden" error. This is
-- of course completely unexepected: who would try to put a newline in a
-- file path? FilePath's implementation just doesn't gaurd against this yet.
-- If a future version does, "unsafe" may be removed from the name below.
unsafeFilePathToLine :: FilePath -> Line
unsafeFilePathToLine = filePathToText .> unsafeTextToLine

-- | Converts a Turtle.FilePath into a *list* of Lines.
-- The "list" part is to handle the possibility of a newline being in the
-- FilePath. Please comment for "unsafeFilePathToLine".
filePathToLines :: FilePath -> [Line]
filePathToLines = filePathToText .> textToLines'

-- As it does in the hostname-validate package:
type Hostname = Data.ByteString.ByteString

-- | Converts a Hostname into a Line. Note the "unsafe":
-- Like unsafeFilePathToLine, we start with a data type whose values
-- *should* not contain newlines: Hostname. However, this is not enforced.
-- But unlike Turtle.FilePath, "Hostname" is merely a type alias, so it is
-- unlikely that a future version of the hostname-validate package will let us
-- take the "unsafe" off of this function.
unsafeHostnameToLine :: Hostname -> Line
-- Notice the "8" below. Hostnames use Word8 vectors.
unsafeHostnameToLine = decodeUtf8 .> unsafeTextToLine

-- | Converts a Hostname into a list of Lines, for the off chance of a newline:
hostnameToLines :: Hostname -> [Line]
-- Please see comment in unsafeHostnameToLine:
hostnameToLines = decodeUtf8 .> textToLines'

isLine :: Text -> Bool
-- If the input does not contain a newline, then it is a line:
isLine = not . (Txt.isInfixOf "\n") -- Harnessing currying

-- | Converts "Text" to a List of "Lines".
-- The Turtle original, "textToLines" uses "NonEmpty" instead of "List".
-- (Notice the "prime" in the function name below.)
textToLines' :: Text -> [Line]
textToLines' = textToLines .> toList

-- | Converts a String into a list of "Lines":
stringToLines :: String -> [Line]
stringToLines = Txt.pack .> textToLines' -- Make it "Text", then make it Lines.

unsafeStringToLine :: String -> Line
unsafeStringToLine = Txt.pack .> unsafeTextToLine

-- | Wraps a Line in single quotes. (Actually apostrophies, accord. to Unicode.)
-- But Bash and just about everything else considers them "single quotes".
singleQuoteLine :: Line -> Line
-- The below is actually safe because we started with a Line and "quote" does
-- not add any newlines. Marshal in, marshal out:
singleQuoteLine = lineToText .> quotes .> unsafeTextToLine

-- | Wraps a Line in double quotes.
doubleQuoteLine :: Line -> Line
-- The below is actually safe because we started with a Line and "doubleQuote"
-- does not add any newlines. Marshal in, marshal out:
doubleQuoteLine = lineToText .> doubleQuotes .> unsafeTextToLine

-- | "Intercalate" equivalent for Lines:
-- (See Data.Text.intercalate or Data.String.Combinators.[ditto] for doc.)
intercalateLines :: Line -> [Line] -> Line
-- The below is actually safe because we are intercalating a Line between
-- a list of Lines. Neither the thing used to intercalate nor the things
-- being conjoined contain a newline, thus the ouput is newline-free.
-- In other words, the output is gauranteed to be a Line. Marshal in & out:
intercalateLines sep list = (Txt.intercalate sep' list')  |>  unsafeTextToLine
                              where sep'  = lineToText sep
                                    list' = map lineToText list

unsafeShowLine :: Show a => a -> Line
unsafeShowLine = show .> unsafeStringToLine
