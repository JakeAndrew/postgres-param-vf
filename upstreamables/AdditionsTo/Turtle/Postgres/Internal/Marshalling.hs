{-# LANGUAGE OverloadedStrings #-}

-- We really only need to export this one function. Everything else in this file
-- is used to define it.
module AdditionsTo.Turtle.Postgres.Internal.Marshalling (pgLinesToPairs) where

-- Using Turtle's FilePath, not Prelude'sL
import Prelude hiding (FilePath)

import Turtle
import Flow
import Data.IP
import Text.Hostname

import AdditionsTo.Turtle.Line
import AdditionsTo.Turtle.Postgres.Internal.Types

{-----------------------------------SUMMARY-------------------------------------

*WARNING* Postgres doc is currently unclear about handling single quotes within
double quotes for unix_socket_directories. This file assumes that the double
quotes already escape the single quotes.
Also, on the side of caution, it is assumed that double quotes cannot be
escaped within double quotes for unix_socket_directories. The doc does not
say how. So if any double quote is given in a value for that parameter, we
print an error message and terminate. This checking-and-erroring is done in
InputCheck.hs.
The following thread is discussing the above matters:
https://www.postgresql.org/message-id/152002095233.21241.1651425970811290805%40wrigleys.postgresql.org .

How we convert file paths, or lack thereof, to a "Line":
  * If given an empty list of file paths, give an empty pair of single quotes.

  * If given one or more file paths, we wrap each in double quotes, separate
      them with comma-spaces, and wrap the whole list in single quotes.
      NB: If a file path contains a double quote or newline, we error. See
      warning above.

  * Note: none of the parameters we have implemented at time-of-writing accept
      a single file path only. We only accept *lists* of file paths.

How we convert hostnames & IP addresses, or lack thereof, to a Line:
    * If given no hostnames or IP addresses, give an empty pair of
        single quotes.

    * If given at least one hostname/IP address, we use the show function to
        convert the IP addresses into strings, which we safely pack as Lines:
        the IP data type is gauranteed to not contain any newlines.
        Hostnames are already Lines. Now that both are Lines, we concatenate
        the two lists. Next, we intercalate that combined list with
        comma-spaces. Lastly, we wrap the whole thing in single quotes. No
        escaping or double quotes are needed: neither hostnames nor IPs may
        contain spaces, single quotes, double quotes or commas. NB: we use the
        validHostname function from the hostname-validate package to validate
        each hostname. If any fail that test, we print an error and terminate.
        But note that testing is done in InputCheck.hs, not here.

    * If given "All" as a value of the "PgAddrs" data type, give '*',
        including the single quotes. That is a Postgres-specific special string.

And Enums/booleans simply become their natural Postgres-English equivalents.
    (True --> "on", Minimal --> "minimal", etc., but w/o quotes.)

-------------------------------- End of Summary--------------------------------}

pgLinesToPairs :: PgLines -> [(Line, Line)]
pgLinesToPairs = map pgLineToPair

pgLineToPair :: PgLine -> (Line, Line)
pgLineToPair pgLine =

    -- Total of 5 cases at time-of-writing.
    -- Please keep in sync with PgLine in Types.hs.
    case pgLine of

{-  1 -} ListenAddresses pgAddrs ->
            "listen_addresses"  `pair`  (pgAddrsPrep pgAddrs)

{-  2 -} UnixSocketDirectories filePaths ->
            "unix_socket_directories"  `pair`  (pgFilePathListPrep filePaths)

{-  3 -} WalLevel walLevel ->
            "wal_level"  `pair`  (walLevelPrep walLevel)

{-  4 -} FSync isFSyncing ->
            "fsync"  `pair`  (pgBoolPrep isFSyncing)

{-  5 -} ArchiveMode archiveMode ->
            "archive_mode"  `pair`  (pgArchiveModePrep archiveMode)

        -- Thanks to the type system, it is impossible to have any other cases,
        -- provided this is kept in sync with the PgConfLine type. So please
        -- compare the two and make sure they match.

pgAddrsPrep :: PgAddrs -> Line
pgAddrsPrep (PgAddrs [] []) = "''"
pgAddrsPrep (PgAddrs hostnames ips) =

    -- After marshalling our input into a simple list of Lines, lineAddrs, we
    -- intercalate that list with comma-spaces, and then wrap it all in
    -- single quotes.
    lineAddrs  |>  (intercalateLines ", ")  |>  singleQuoteLine

      -- Turn IP list into a Line list:
      -- "unsafeShowLine" is like "show", but gives a Line. It is safe here
      -- because IPs are gauranteed by their type to not contain any newlines.
      where lineIPs = map unsafeShowLine ips

            -- Turn hostname list into a Line list:
            lineHostnames = map unsafeHostnameToLine hostnames

            -- Let's put the two together:
            lineAddrs = lineHostnames <> lineIPs

pgAddrsPrep All = "'*'" -- Special Postgres config string for "All"

pgBoolPrep :: Bool -> Line
pgBoolPrep True  = "on"
pgBoolPrep False = "off"

walLevelPrep :: WalLevelEnum -> Line
walLevelPrep Minimal = "minimal"
walLevelPrep Archive = "achive"
walLevelPrep HotStandBy = "hot_standby"
walLevelPrep Logical = "logical"

pgArchiveModePrep :: PgArchiveMode -> Line
pgArchiveModePrep Off = "off"
pgArchiveModePrep On  = "on"
pgArchiveModePrep Always = "always"

pgFilePathPrep :: FilePath -> Line
-- Convert to a "Line" and then wrap in double quotes:
-- We need to confirm beforehand that the file path does not contain a newline,
-- or else the unsafe function can throw an error.
pgFilePathPrep = unsafeFilePathToLine .> doubleQuoteLine

pgFilePathListPrep :: [FilePath] -> Line
pgFilePathListPrep []   = "''"
-- Convert paths to Lines and individually double-quote (the map part), then put
-- a comma-space between each, then wrap the whole thing in single quotes.
pgFilePathListPrep xs   =
    (map pgFilePathPrep xs)  |>  (intercalateLines ", ")  |>  singleQuoteLine

-- | Makes an ordered pair. Intended for use with backticks to reduce parens.
pair :: a -> b -> (a, b)
pair x y = (x, y)
