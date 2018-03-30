{-# LANGUAGE OverloadedStrings #-}

module AdditionsTo.Turtle.Postgres.Internal.InputCheck
    -- Please note the below is only internally exported. We use it to make a
    -- class instance in Internal.hs. "pgLinesVerificationReport" is not
    -- exported by AdditionsTo.Turtle.Postgres.
    (pgLinesVerificationReport) where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Turtle
import Filesystem.Path.CurrentOS
import Flow
import Text.Hostname -- From hostname-validate package.
import Data.IP

import AdditionsTo.Turtle.Postgres.Internal.Types  -- To get PgLines, etc.
import AdditionsTo.Turtle.Prelude -- To get "VerificationReport," etc.
import AdditionsTo.Turtle.Line


pgLinesVerificationReport :: PgLines -> VerificationReport
pgLinesVerificationReport =
    -- Build the report list, then concatenate it together.
    (map pgLineVerificationReport) .> catVeriReports

pgLineVerificationReport :: PgLine -> VerificationReport
pgLineVerificationReport x =
    case x of
        ListenAddresses addrs ->  pgAddrsReport addrs
        UnixSocketDirectories filePaths -> filePathsReport filePaths
        _ -> Passed -- Gauranteed by type system.

pgAddrsReport :: PgAddrs -> VerificationReport
-- Note the type system gaurantees that the IP addresses are valid, so we
-- don't have to check them, hence the underscore.
pgAddrsReport (PgAddrs hostnames _ ) =
    -- Build the report list, then concatenate it together.
    (map hostnameReport hostnames)  |>  catVeriReports

hostnameReport :: Hostname -> VerificationReport
hostnameReport hostname =
    if (validHostname hostname) then Passed
    else FailedBecause ( hostname' <> ["is not a valid hostname."] )
      where hostname' = hostnameToLines hostname

filePathsReport :: [FilePath] -> VerificationReport
filePathsReport = (map filePathReport) .> catVeriReports

filePathReport :: FilePath -> VerificationReport
filePathReport x

    -- We don't support file paths that contain double quotes. Please see the
    -- block comment at the top of Marshalling.hs.
    | '\"' `elem` filePath =
        FailedBecause ( ["The following file path:"] <> filePathAsLines <>
                        ["contains a double quote. That is not supported."]
                      )

    -- And if a file path contains a newline, that's definitely not supported.
    -- Postgres needs all key-value pairs to go on one line in the config file,
    -- and it does not document anyway of signalling that character.
    | '\n' `elem` filePath =
        FailedBecause ( ["The following file path:"] <> filePathAsLines <>
                        ["contains a newline. That is not supported."]
                      )

    | otherwise = Passed

      where
          filePath = encodeString x -- Encode "FilePath" x as a String.

          -- Though a very bizzare thought, it is technically possible for
          -- a "FilePath" to contain a newline, at time-of-writing. Hopefully,
          -- a future implementation of Turtle's FilePath will make that
          -- impossible. But for now, we must admit the possibility and marshal
          -- the FilePath into a *list* of Lines:
          filePathAsLines = stringToLines filePath
