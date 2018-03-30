-- NB: class instance implicitly exported. Please see 2nd paragraph here:
-- https://wiki.haskell.org/Orphan_instance .
module AdditionsTo.Turtle.Postgres.Internal.Types
    ( PgLine (ListenAddresses, UnixSocketDirectories, WalLevel, FSync, ArchiveMode)
    , PgLines
    , PgAddrs (All, PgAddrs)
    , Hostname
    , noAddrs, pgHostname, pgHostnames, pgIP, pgIPs -- For readability/cleanliness
    , WalLevelEnum (Minimal, Archive, HotStandBy, Logical)
    , PgArchiveMode (Off, On, Always)
    ) where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Turtle
import Data.IP
import Data.ByteString.Internal -- What the hostname-validate package uses.
import Data.Text


{-
   Let a "Postgres Line", PgLine, be a Line that we put into a Postgres config
   file. The Line either consists of a "key" and a "value", or a key and a list
   of zero or more values. On the Haskell side of things, the type constructor
   takes the place of the key. This makes conversion to "Line" easy and
   type-appropriate via pattern matching, done in Marshalling.hs.

   NB: At time of writing, we only implement what we need for the sdb program.
   So as you can see, not every possible option that can go into postgresql.conf
   is mentioned in our "PgLine" data type below. Please add to it as needed.

   Also note that "Comment" is not a constructor for PgLine. That would break
   the mold of a key being followed by a value and would complicate
   implemenation. Instead, you may use Haskell code comments, make a Markdown
   file about your config settings, or even put the comment into the config file
   yourself. That last option is valid, since modifyPostgresConf does not delete
   any existing comments and preserves the order of lines.
-}
data PgLine =
    -- Total of 5 options used at time of writing.
    -- Please check w/ pgLineToPairs in "Marshalling".
    ListenAddresses PgAddrs             -- 1  (See below about "PgAddrs".)
    | UnixSocketDirectories [FilePath]  -- 2
    | WalLevel WalLevelEnum             -- 3
    | FSync Bool                        -- 4
    | ArchiveMode PgArchiveMode         -- 5

{-
   We'll also establish the plural form of the above type below.
   NB: in Internal.hs, we make the below an instance of the "Verifiable" class,
   using a function from InputCheck.hs. We can't make that instance in either
   this file or InputCheck.hs; that would make a module cycle.
   The Verifiable class is from AdditionsTo.Turtle.Prelude.
-}
type PgLines = [PgLine]

-------------------------- PgAddrs type & company ------------------------------
type Hostname = Data.ByteString.Internal.ByteString

{-
   The "Postgres Addresses" data type, PgAddrs, is either the special value
   "All" or a list of hostnames plus a list of IP addresses.

   NB: We have the potential for unwanted empty brackets here. Suppose the
   programmer has only hostnames or only IP addresses, or nothing at all (this
   last one *is* legal). Those cases would result in one or more empty lists:
   clutter we'd rather not have in our code. So we offer the below "cleanliness"
   functions and value to avoid empty brackets. But we have a fundamental
   limitation here: no matter what we do, we cannot *force* the programmer to
   not use empty brackets. The best we can do is to politely ask them to use the
   approriate cleanliness function/value below. Even if we used the "NonEmpty"
   list type constructor, the programmer could still contruct a singleton using
   a value and an empty list.
-}
data PgAddrs = All | PgAddrs [Hostname] [IP]

noAddrs :: PgAddrs
noAddrs = PgAddrs [] [] -- No hostnames, no IP addresses.

pgHostname :: Hostname -> PgAddrs
pgHostname hostname = PgAddrs [hostname] []

pgHostnames :: [Hostname] -> PgAddrs
pgHostnames hostnames = PgAddrs hostnames []

pgIP :: IP -> PgAddrs
pgIP ip = PgAddrs [] [ip]

pgIPs :: [IP] -> PgAddrs
pgIPs ips = PgAddrs [] ips

------------------------ End of PgAddrs type & company -------------------------

-- NB: Written from least to most verbose: (WAL = Write-Ahead Log)
-- "Enum" added to disambiguate from PgLine type constructor "WalLevel".
data WalLevelEnum = Minimal | Archive | HotStandBy | Logical

-- "Pg" prefix mainly added to disambiguate for PgLine type constructor.
-- In theory, it could also prevent a naming conflict.
data PgArchiveMode = Off | On | Always
