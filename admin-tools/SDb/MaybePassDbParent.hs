{-# LANGUAGE OverloadedStrings #-}

-- The database parent directory, abbreviated as dbParent, is where we place
-- the database directory, .postgres-work. Or, dbParent is where we look for
-- it, if the database directory is already established.

-- Currently, we are hard-coding dbParent to be the project root, meaning,
-- we always place, or look for, .postgres-work in the project root.
-- However, we may not be able to obtain the project root's path, hence the
-- "maybe" prefixes.

-- This file exists to define "maybePassDbParentTo". Since we are currently
-- hard-coding the database parent directory to be the project root, we
-- basically define "maybePassDbParentTo" by currying "maybePassProjRoot", from
-- the local "upstreamables" package, to use the error messages we give here.

module SDb.MaybePassDbParent (maybePassDbParentTo) where

-- Using Turtle's FilePath, not Prelude's
import Prelude hiding (FilePath)

import Turtle

import AdditionsTo.Stack.Path.MaybeGetProjRoot
import AdditionsTo.Turtle


-- ##
-- ## Plain, Simple Constants
-- ##

-- Used to tell if the path given by `stack path --project-root` is actually
-- the Snowdrift project root. i.e., does the following file exist in the
-- reported directory?:
projRootIndicatorFromThisFile :: FilePath
projRootIndicatorFromThisFile = "website/Snowdrift.cabal"


-- ##
-- ## Fancy, Complicated Constants
-- ##

-- | Here, we curry maybePassProjRoot with our particular error messages.
-- Please see comment at top of file.
maybePassDbParentTo :: MonadIO io => (FilePath -> IO ()) -> io ()
maybePassDbParentTo = maybePassProjRoot projRootErrorInfo
  where projRootErrorInfo = ProjRootErrorInfo { execNotFound = noStackExecError, cantFindProjRootError = cantFindProjRootErrorFromThisFile, projRootIndicator = projRootIndicatorFromThisFile }


-- | Prints the error for if "stack" is unavailable:
noStackExecError :: Shell ()
noStackExecError = do

    progName <- getProgName'

    mapM_ err
        [ progName <> " has aborted: could not find the executable, \"stack\"."
        , progName <> " needs \"stack\" to find Snowdrift's project"
        , "directory: that is where the program puts, or looks for, the database"
        , "directory. Please install the Haskel Tool Stack / make sure"
        , "\"stack\" is visible to " <> progName <> "."
        , ""
        , "NB: if you're running " <> progName <> " through Stack, then"
        , "\"stack\" needs to be installed in itself, i.e., the command"
        , "\"stack exec -- stack --help\" needs to work."
        ]

-- | Prints the error for if this program was ran outside of the project root.
-- i.e., `stack path --project-root` gave the "wrong" answer because we were
-- in the wrong directory. If its answer didn't look like the project root to
-- us, we give this error message:
cantFindProjRootErrorFromThisFile :: FilePath -> Shell ()
cantFindProjRootErrorFromThisFile reportedProjRoot = do

    progName <- getProgName'

    -- Safety is ensured by you checking the hard-coded value for newlines:
    let pRIFTH = unsafeFilePathToLine projRootIndicatorFromThisFile

    let
      messageLines =
        [ progName <> " has aborted. It appears to have been ran from outside the"
        , "Snowdrift project directory. It's looking for"

        -- Why do we share this implementation detail with the user? We do so
        -- for maintainance debugging: if this program stops working because the
        -- indicator file was renamed or moved, we'd want the error to inform us
        -- of that.
        , "\"[reported root]\"/" <> pRIFTH <> " as its indicator of whether"
        , "\"[reported root]\" is actually the project root."
        , "This program must be ran from within the project directory to find that"
        , "root, which is where " <> progName <> " puts, or looks for, the"
        , "database directory. Please change your current directory to be in the"
        , "Snowdrift project and then try running the previous command again."
        , ""
        , "The reported project root is:"
        ]
        <> filePathToLines reportedProjRoot

    mapM_ err messageLines
