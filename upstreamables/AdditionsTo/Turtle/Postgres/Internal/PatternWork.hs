-- Please reference the Turtle.Pattern doc and the relevant doc in
-- Turtle.Prelude, searching for "Pattern" in the latter.

-- In functional programming, Patterns don't actually do anything, but we speak
-- as though they do for brevity: functions accept a pattern and text and output
-- the resulting text. But we speak as though the Pattern altered the text.
-- *Key to note*: Turtle's Patterns do not simply say whether or not the given
-- text matches. With functions like Turtle's "sed", the Pattern can replace
-- the matching portion of text with something else. When making a Pattern, by
-- default, the Pattern replaces matches with whatever matched, resulting in no
-- change. This may be altered with the ">>" operator.

{-# LANGUAGE OverloadedStrings #-}

module AdditionsTo.Turtle.Postgres.Internal.PatternWork
    (modifyPostgresConf') where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Control.Exception.Base (bracket)
import Data.Text (Text)
import Turtle hiding (need, opt)

import AdditionsTo.Turtle.Line
import AdditionsTo.Turtle.Patterns

-- | Applies settings to an existing Postgres config file.
-- Note we have already marshalled the PgLines into a list of Line pairs.
-- We did that in ../../ModifyPostgresConf.hs. Also there, we've already checked
-- that the options, now Line pairs, are valid. Note that if we encounter a
-- Line in the file that does not match anything in the given list of options,
-- we leave it as-is. That behavior is dictated by "inplace" in "lineInplace".
-- Thus any Line in the file that is a comment, whitespace, or an option we
-- didn't specify in the given options, is let be.
modifyPostgresConf' :: MonadIO io => [(Line, Line)] -> FilePath -> io ()
modifyPostgresConf' writeThese writeHere =

    -- Please read below about "optsToPatterns" and "choice".

    -- "lineInplace" goes line-by-line through a file. If a Line matches the
    -- Pattern, it replaces the Line as the Pattern dictates, just like
    -- regular "inplace", except it accepts a "Pattern Line".
    lineInplace (choice (optsToPatterns writeThese)) writeHere

-- | Converts marshalled PgLines into Patterns useful for config file editing.
-- Above, we use "choice" to treat the outputted list of Patterns as one, big,
-- prioritized Pattern.
optsToPatterns :: [(Line, Line)] -> [Pattern Line]
optsToPatterns opts = map optToPattern opts

-------------------------- Defining "optToPattern" -----------------------------

-- | Takes an option (key-value pair) and converts it into a Pattern.
-- The generated Pattern recognizes a Postgres Line that sets a value for the
-- given key, and replaces the matched Line with the key-value pair supplied,
-- thus updating the value. Even if the value is already the way we want it, if
-- the Line matched, we replace it. This ensures consistent formatting: we put
-- an optional equals sign between the key and value and have one space on
-- either side of the equals sign.
optToPattern :: (Line, Line) -> Pattern Line
optToPattern (key, value) =
    -- The Pattern resulting from "optToPattern (k, v)" has the same matching
    -- criteria as that of "optMatcher k", it's just that the latter returns
    -- the matching Line, whereas the former returns what we want to replace it
    -- with. Note that it only replaces anything upon a match.
    (optMatcher key) >> pure (key ..=.. value)

optMatcher :: Line -> Pattern Line
optMatcher key =
    -- We have a match if and only if the Line has any amount of whitespace
    -- followed by the key exactly, followed by anything (or nothing).
    -- Note we really don't care what comes after the key. If the key matches,
    -- we're replacing the line.
    -- Below, "line" is analogus to "text" from Turtle.Pattern, and "lineChars"
    -- is like "chars" from the same module. Note the ">>" concatenates together
    -- an "abstract string" (technically, an abstract "Line").
    lineWhitespace >> line key >> lineChars

------------------------- End of Defining "optToPattern" -----------------------

-- | Joins two strings with an equals sign and spaces.
(..=..) :: (IsString str, Monoid str) => str -> str -> str
(..=..) leftStr rightStr = leftStr <> " = " <> rightStr
