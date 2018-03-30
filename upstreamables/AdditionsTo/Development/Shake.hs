module AdditionsTo.Development.Shake ((%>>), removeFilesAfter', need') where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Development.Shake
import Filesystem.Path.CurrentOS -- Turtle's FilePath

import AdditionsTo.Filesystem.Path.CurrentOS


-- | Like Shake's "%>", but takes a Turtle.FilePath, and an "Action ()":
(%>>) :: FilePath -> Action () -> Rules ()
-- Encode the FilePath into a string (which works as a "FillePattern") and
-- assume the given action, regardless of further input from %> , hence us
-- accepting an "Action ()" instead of a "FilePath -> Action()".
(%>>) filePath action = (encodeString filePath) %> (const action)

-- | Like Shake's "removeFilesAfter", but takes a Turtle.FilePath:
removeFilesAfter' :: FilePath -> [FilePattern] -> Action ()
removeFilesAfter' filePath patterns =

    -- Simply encoding the FilePath as a string and passing the arguments off
    -- to the original, analogous function:
    removeFilesAfter (encodeString filePath) patterns

-- | Like Shake's "need", but takes a Turtle.FilePath:
need' :: [FilePath] -> Action()
need' = need . (map filePathToString)
