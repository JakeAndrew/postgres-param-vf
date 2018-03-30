{-# LANGUAGE OverloadedStrings #-}

module SDb.ModifyPostgresConf (simpleModifyPostgresConf) where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Turtle

-- We really just want AdditionsTo.Turtle.Postgres, but GHC can only find
-- the "grandparent" module from here:
import AdditionsTo.Turtle

-------------------------- Mostly "Hard-Coded" Values --------------------------

-- | These are the settings we want written into Postgres's config file.
-- "writeHere" dictates the name of the file.
writeThese :: FilePath -> [PgLine] -- Or PgLines: PgLines equals [PgLine].
writeThese pghost =
    [
      -- set the unix socket directory because pg_ctl start doesn't
      -- pay attention to PGHOST (wat.)
      UnixSocketDirectories [pghost]
    , ArchiveMode Off
    , FSync False
    , WalLevel Minimal
      -- don't bother listening on a port, just a socket, signified by
      -- the empty list.
    , ListenAddresses noAddrs
    ]

-- | The file name of the config file we're writing:
writeHere :: FilePath
writeHere = "postgresql.conf"
----------------------- End of "Mostly Hard-Coded Values" ----------------------

simpleModifyPostgresConf :: MonadIO io => FilePath -> FilePath -> io ()
simpleModifyPostgresConf pghost pgdata =

    modifyPostgresConf writeThese' writeHere'

      where
          writeThese' = writeThese pghost
          writeHere'  = (pgdata </> writeHere)
