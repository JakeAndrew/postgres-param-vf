module AdditionsTo.Filesystem.Path.CurrentOS
    ( filePathToText
    , filePathToString
    ) where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Turtle
import Data.Text as Txt

-- | Simply converts a "Turtle.FilePath" into "Text". Formerly "toText_".
filePathToText :: FilePath -> Text
filePathToText = format fp

-- | Converts a "Turtle.FilePath" into a "Prelude.FilePath", a String:
filePathToString :: Turtle.FilePath -> String
filePathToString = Txt.unpack . filePathToText
