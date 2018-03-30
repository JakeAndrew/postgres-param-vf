-- Lets us have a sub-module named "Prelude":
{-# LANGUAGE NoImplicitPrelude #-}

module AdditionsTo.Turtle  -- "Turtely enough for your turtle club?"
    ( module Line                          -- Master of Disguise
    , module Prelude
    , module Shell
    , module Bash
    , module Postgres
    , module Shake
    ) where

-- Modules that add to exiting modules:
import AdditionsTo.Turtle.Line     as Line
import AdditionsTo.Turtle.Prelude  as Prelude
import AdditionsTo.Turtle.Shell    as Shell

-- Modules without parallel:
import AdditionsTo.Turtle.Bash     as Bash
import AdditionsTo.Turtle.Postgres as Postgres
import AdditionsTo.Turtle.Shake    as Shake
