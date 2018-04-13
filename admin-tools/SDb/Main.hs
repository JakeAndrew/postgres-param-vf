{-# LANGUAGE OverloadedStrings #-}

--  | This program is "sdb", which stands for "start database".
--  But note it can also stop or clean the database. You may be able to read it
--  more smoothly as "_S_nowdrift _d_ata_b_ase [command]".
--  Please scroll down to "usageText" for more info.

-- Please see MainNotes.md about the "hiding" below.
import Prelude hiding (FilePath, print, printf, Text.Printf, hPrint, hprintf)

-- Modules from base:
import System.Environment (getArgs)

-- Modules from Hackage, excluding those from base:
import Data.Text (Text)
--
-- Replacing hidden functions with Turtle.FilePath-accepting variants:
import Development.Shake hiding ((%>), removeFilesAfter)
--
import Turtle hiding (need, opt, echo) -- "need" conflicts with Shake's "need".
import qualified Data.Text as Txt
import qualified Turtle

-- Local Modules:
import SDb.MaybePassDbParent
import SDb.ModifyPostgresConf
import AdditionsTo.Turtle
import AdditionsTo.Development.Shake -- need', "%>>" and removeFilesAfter'
import AdditionsTo.Stack.Path.MaybeGetProjRoot
import AdditionsTo.Filesystem.Path.CurrentOS


----------------------------- Simple Constants --------------------------------

pgWorkDir :: FilePath
pgWorkDir = ".postgres-work"  -- This is where we put the database files.
                              -- At time of writing, we place this directory
                              -- right in the project root.

dbRunning, dbCluster :: FilePath
dbRunning = pgWorkDir </> "data/postmaster.pid"
dbCluster = pgWorkDir </> "data/postgresql.conf"

data Databases = DBS { dbsMain :: Text, dbsTest :: Text }
dbnames :: Databases
dbnames = DBS "snowdrift" "snowdrift_test"

--------------------------- End of Simple Constants ----------------------------

usageText :: Shell ()
usageText = do

    progName <- getProgName'

    mapM_ err
        [  progName <> ": provides basic control over the Snowdrift database server,"
        , "such as starting and stopping."
        , ""
        , "Please see BUILD.md and \"build.sh help\": sometimes you need to run"
        , "build.sh instead of \"" <> progName <> " start\". That doc will tell you\
        \ when."
        , ""
        , "Usage:"
        , ""
        , "    " <> progName <> " ACTION"
        , ""
        , "Where ACTION may be one of:"
        , ""
        , "    clean             \"rm -rf\" the whole cluster"
        , "                             Note: this also terminates the server"
        , "                             process."
        , ""
        , "    help              print this text"
        , ""
        , "    env               print export commands for PGHOST and PGDATA"
        , "                             e.g. 'source <(" <> progName <> " env)'"
        , ""
        , "    start             start the cluster"
        , "                             Note: this automatically sets up environment"
        , "                             variables and the dev/test databases for"
        , "                             hacking on the Snowdrift.coop website."
        , ""
        , "    stop              stop the cluster"
        ]

-- | Here, we branch execution by command-line arg for the first time.
-- Note we delegate some branching to argCasesTwo.
argCasesOne :: FilePath -> IO ()
-- dbParent stands for "database parent directory". Please see comments at the
-- top of MaybePassDbParent.hs.
argCasesOne dbParent = sh $ do  -- Entering Turtle Shell context. See type of "sh".

    -- Set up envirionment variables:
    (pghost, pgdata) <- initEnv dbParent -- Just keep passing dbParent.

    args <- liftIO getArgs

    -- If we can easily perform the command without Shake, and no other Shake
    -- Actions need the command, we do it in here:
    case args of

        ["env"] -> do -- If we got the "env" arg, and it alone...

            -- ... print Bash export commands. Note this program does not
            -- execute them, but they could be executed from a Bash script
            -- using the "source" command.
            printExportFilePath "PGHOST" pghost
            printExportFilePath "PGDATA" pgdata
            printExport "PGDATABASE" (dbsMain dbnames)

        -- If we got the "pg_ctl" arg, possibly followed by more args, run
        -- pg_ctl, passing it any remaining args:
        ("pg_ctl":args') -> procs "pg_ctl" (map Txt.pack args') empty

        -- We delegate all other cases to argCasesTwo, which accesses the
        -- commandline args itself, so we don't pass in "args".
        _ -> liftIO (argCasesTwo pghost pgdata)

-- | This is where we do our remaining branching-by-argument.
-- NB: The below is a "creative" use of the Shake Build System: we don't
-- actually compile anything. Instead, we use Shake's "Rules" monad and "phony"
-- function to branch on args. Also, we use Shake's "need" function to wait on
-- certain files to come into existence before continuing.
argCasesTwo :: FilePath -> FilePath -> IO ()
argCasesTwo pghost pgdata = shakeArgs shakeOptions $ do--Entering Rules context,
                                                       -- see type of shakeArgs.

    want ["help"] -- Makes sdb display the help text when given no cmdline args.

    phony "help" $ actsh usageText

    phony "clean" $ do -- Entering Shake's "Action" context, see type of "phony".
        need' ["stop"]  -- "need"ing a phony does the phony. (no "stop" file)
        removeFilesAfter' absolutePgWorkDir ["//*"]

    -- To start the database, we need to wait for the absoluteDbRunning file
    -- to come into existance, which we make happen with the two "%>>" lines.
    phony "start" (need' [absoluteDbRunning])

    -- The "stop" cmdline arg prints Postgres's status and then stops the
    -- process:
    phony "stop" $ actsh (pgStatus .&&. shell "pg_ctl stop" empty)

    {-
        The below do not establish recognized commandline args, but the "start"
        arg uses them.
    -}

    -- Using "%>>" instead of "phony" because the latter would make
    -- absoluteDbRunning a legal commandline argument.
    absoluteDbRunning %>> need' [absoluteDbCluster]

    absoluteDbCluster %>> (actsh $ initCluster pghost pgdata)

    -- Assuming that pghost is always a grandchild of pgWorkDir, we define
    -- absolutePgWorkDir below instead of passing it in as a third param. This
    -- keeps "argCasesTwo" in second normal form.
      where absolutePgWorkDir = (parent . parent) pghost
            absoluteDbRunning = absolutePgWorkDir </> dbRunning
            absoluteDbCluster = absolutePgWorkDir </> dbCluster

initCluster :: FilePath -> FilePath -> Shell ()
initCluster pghost pgdata = do
    step "Creating directories..."
    mktree pghost
    mktree pgdata

    step "Initializing cluster..."
    let pgdata' = filePathToText pgdata
        in hush $ procs "pg_ctl" ["initdb"] empty

    --debug:
    err $ "Writing postgresql.conf to: " <> (unsafeFilePathToLine pgdata)

    step "Updating cluster configuration file..."
    liftIO (writeTextFile (pgdata </> "postgresql.conf")
        "unix_socket_directory\nunix_socket_directories\nlisten_addresses")
    -- Yet again, we pass pghost and pgdata.
    -- Remaining details delegated to module. Please see ModifyPostgresConf.hs.
    simpleModifyPostgresConf pghost pgdata

    step "Starting database server..."
    procs "pg_ctl" ["start", "-w"] empty

    step "Creating databases..."
    procs "createdb" [dbsMain dbnames] empty
    procs "createdb" [dbsTest dbnames] empty

    step "Success."

-- | Create and export some env variables
initEnv :: FilePath -> Shell (FilePath, FilePath)
initEnv dbParent = do  -- See comments in mainPart.
    pgPath <- getPgExecsPath
    let pghost = dbParent </> pgWorkDir </> "sockets"
        pgdata = dbParent </> pgWorkDir </> "data"
    exportFilePath "PGHOST" pghost
    exportFilePath "PGDATA" pgdata
    export "PGDATABASE" (dbsMain dbnames)
    addToPath pgPath
    return (pghost, pgdata)

-- First, we see if the needed Postgres executables are available to us. If they
-- aren't, we print an error and terminate. If they are, we try to get the
-- database parent directory path. If sucessful again, we pass the path on to
-- the rest of the program. If not, "maybePassDbParentTo" prints an
-- error message.
main :: IO ()
main = pgExecsTest $ maybePassDbParentTo argCasesOne
