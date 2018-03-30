{-# LANGUAGE OverloadedStrings #-}

module AdditionsTo.Turtle.Bash (printExport, printExportFilePath) where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Turtle
import Data.Text

import AdditionsTo.Turtle.Line
import AdditionsTo.Filesystem.Path.CurrentOS
import AdditionsTo.System.Posix.Escape

-- | Prints a Bash export command, given a variable name & value.
-- Note that the Haskell program does not execute the command, but Bash may. For
-- example, a Bash script may feed the command into "source": that would do it.
-- Important: no space on either side of the equals sign.
printExport :: MonadIO io => Line -> Text -> io ()
printExport name val =

    if isLine val then -- Makes "unsafeTextToLine" safe.
        echo (mconcat ["export ", name, "=", unsafeTextToLine val'])
    else
        -- Cons literal to list of lines of val'. Use err on each line,
        -- including the literal, as one action.
        mapM_ err ("Tried to printExport a string containing a newline. The string is:" : textToLines' val')

      where val' = escapeText val

-- | Like "printExport", but accepts a Turtle.FilePath for the variable's value.
-- *Warning*: we're assuming that FilePaths do not contain newlines.
-- Given a codebase with this check built-in, our assumption would be
-- gauranteed. But at time of writing, newlines are allowed into FilePaths.
-- If given a FilePath containing a newline, this function would throw a
-- NewLineForbidden exception.
printExportFilePath :: MonadIO io => Line -> FilePath -> io ()
printExportFilePath name val = printExport name (filePathToText val)
