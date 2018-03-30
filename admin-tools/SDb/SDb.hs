-- This file exists for organization only: it just groups together the three
-- parts of the sdb program. Note that module "Main" is implicitly declared in
-- Main.hs, not to be confused with lowercase "main".
module SDb ( module Main
           , module MaybePassDbParent
           , module ModifyPostgresConf
           ) where -- The "where" is a syntactic artifact.

import Main
import MaybePassDbParent
import ModifyPostgresConf
