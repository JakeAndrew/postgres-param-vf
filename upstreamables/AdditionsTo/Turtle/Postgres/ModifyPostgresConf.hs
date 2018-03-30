{-# LANGUAGE OverloadedStrings #-}

module AdditionsTo.Turtle.Postgres.ModifyPostgresConf (modifyPostgresConf) where

{-
   In this file, we focus on the overall logic:

    1) Validate the input.

    2) If it's good, delegate the task of actually modifying the config file.
        Please see the Internal/PatternWork directory for info on that.

    3) If it's bad, print the reported error to standard error.
-}

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Turtle
import qualified Data.Text as Txt

import AdditionsTo.Turtle.Prelude
import AdditionsTo.Turtle.Line
import AdditionsTo.Turtle.Postgres.Internal
import AdditionsTo.Filesystem.Path.CurrentOS

-- | Applies settings to an existing Postgres config file, if all is good.
-- Note that if some setting "foo = bar" pre-exists in the file, and we
-- do not mention it as a parameter to modifyPostgresConf, it stays as-is: it is
-- *not* like we wipe the file and start over.
modifyPostgresConf :: MonadIO io => PgLines -> FilePath -> io ()
modifyPostgresConf writeThese writeHere =

    -- Let's get a report on the given options:
    case (verificationReport writeThese) of

        -- If the report is "Passes", then behave in the expected way:
        -- marshal the given options and then delegate:
        Passed -> modifyPostgresConf' (pgLinesToPairs writeThese) writeHere

        -- But if the given options fail the test because of x, print x as an
        -- error. ("x" is a list of Lines, hence the "mapM_.")
        FailedBecause x -> mapM_ err x -- "err" is from Turtle.Prelude.
