{-# LANGUAGE OverloadedStrings #-}

module AdditionsTo.Turtle.Postgres.Executables
    ( pgExecsTest
    , getPgExecsPath
    , pgStatus
    ) where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Turtle

import AdditionsTo.Turtle.Line
import AdditionsTo.Turtle.Prelude

-- | Evals to param if pg_ctl and pg_config are available. If not, prints error.
-- The Monoid constraint gives us "no-op" as a default value for "io ()", used
-- for conditional error messages in the "no error" case.
pgExecsTest :: ( MonadIO io, Monoid (io ()) ) => io () -> io ()
pgExecsTest action = do

    havePgCtl <- testexec pgCtl -- Is the "pg_ctl" command present/visible?
    havePgConf <- testexec pgConf -- Is "pg_config", too?

    -- If they are, output the given action:
    if havePgCtl && havePgConf then action

        else do  -- Otherwise, print the appropriate error(s):
            progName <- getProgName'

            -- "!>" prints the error when needed, does nothing otherwise.
            -- NB: "!>" will itself error if given a string containing a newline.
            (not havePgCtl)  !> (oneLinerErr pgCtl')
            (not havePgConf) !> (oneLinerErr pgConf')

            mapM_ err
                [ "Please make sure the above Postgres command(s) is/are "
                , "installed/visible to " <> progName <> "."
                ]

      where
          oneLinerErr missing =
            "The Postgres command, \"" <> missing <> "\", is not available."

          pgCtl = "pg_ctl"
          pgConf = "pg_config"

          -- The above two must be FilePaths to work with "testexec".
          -- Below are the "Line" variants:
          pgCtl' = unsafeFilePathToLine pgCtl
          pgConf' = unsafeFilePathToLine pgConf

-- | Haskell Wrapper for "pg_config --bindir". Assumes the command is available.
getPgExecsPath :: Shell FilePath
getPgExecsPath = lineToFilePath <$> inshell "pg_config --bindir" ""

-- | Prints Postgres status and returns an exit code. Assumes pg_ctl is around.
pgStatus :: MonadIO io => io ExitCode
pgStatus = shell "pg_ctl status" empty
