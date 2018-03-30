-- Lets PgLines be an instance of of a type class:
{-# LANGUAGE FlexibleInstances #-}

-- NB: the only things here that get exported by AdditionsTo.Turtle.Postgres are
-- the contents of "Types" and the class instance declared in this file.
module AdditionsTo.Turtle.Postgres.Internal
    ( module Marshalling
    , module InputCheck
    , module Types
    , module PatternWork
    -- Note the implicitly exported class instance.
    ) where

import AdditionsTo.Turtle.Postgres.Internal.Marshalling as Marshalling
import AdditionsTo.Turtle.Postgres.Internal.InputCheck as InputCheck
import AdditionsTo.Turtle.Postgres.Internal.Types as Types
import AdditionsTo.Turtle.Postgres.Internal.PatternWork as PatternWork

import AdditionsTo.Turtle.Prelude -- How we get the "Verifiable" type class.

-- Here, we declare which function we use to verify PgLines. PgLines are from
-- module "Types".
-- (How we make sure they're legal, or at least give an error otherwise.)
instance Verifiable PgLines where
    verificationReport = InputCheck.pgLinesVerificationReport
