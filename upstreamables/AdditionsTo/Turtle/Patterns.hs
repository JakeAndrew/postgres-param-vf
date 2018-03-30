{-# LANGUAGE OverloadedStrings #-}

module AdditionsTo.Turtle.Patterns
    ( lineInplace
    , lineWhitespace
    , line
    , lineChars
    ) where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Turtle
import Flow

-- | Just like "inplace", except it takes a "Pattern Line":
lineInplace :: MonadIO io => Pattern Line -> FilePath -> io ()
lineInplace line path = inplace (lineToText <$> line) path

-- | Just like "spaces", but for Lines:
lineWhitespace :: Pattern Line
lineWhitespace =
    -- This is actually safe because we can only use a "Pattern Line" with other
    -- Line things, unless we delibertly break that.
    unsafeTextToLine <$> spaces

-- | Just like "text", but for Lines:
line :: Line -> Pattern Line
line =
    -- Please see comment in lineWhitespace about saftey.
    -- Given line is converted to Text, then given to "text" to make a
    -- "Pattern Text", which is then transformed into a "Pattern Line":
    lineToText .> text .> (fmap unsafeTextToLine)

    -- Extra credit: if you want to match the way the others are written,
    -- you may use the following instead, since "<$>" for functions is just
    -- composition:
    -- (fmap unsafeTextToLine) <$> (lineToText .> text)

-- | Just like "chars", but for Lines:
lineChars :: Pattern Line
lineChars = unsafeTextToLine <$> chars -- Please see comment in lineWhitespace.
