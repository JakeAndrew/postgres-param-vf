{-# LANGUAGE OverloadedStrings #-}

module AdditionsTo.Turtle.Prelude
    ( testexec
    , getProgName'
    , exportFilePath
    , addToPath
    , step
    , (!>)
    , (?>)
    , VerificationReport (Passed, FailedBecause), Verifiable, verificationReport
    , allPassed, catVeriReports
    ) where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import System.Environment (getProgName)
import Data.Maybe

-- "when" conflicts with Turtle's "when". Module imported for "?<>", which gives
-- its right arg on true and a default value on false.
import Control.Conditional hiding (when)
--import Data.Text as Txt hiding (null, all)
import Turtle -- Pulls "MonadIO"

import AdditionsTo.Filesystem.Path.CurrentOS
import AdditionsTo.Turtle.Line

-- | Check if an executable exists.
-- Here, we're following the naming convention of "testfile", "testdir", etc.
testexec :: MonadIO io => FilePath -> io Bool
testexec exec = do   -- "exec" is the name of the executable in question.

    -- If "which" can find a given executable, it gives "Just" its file path.
    -- Otherwise, it gives "Nothing".

    maybePath <- which exec

    -- So, if "maybePath" actually is something (not Nothing)...
    if (isSomething maybePath)

        -- then the executable does exist, so we return True:
        then return True

        -- Otherwise, we got "Nothing", so the executable does not exist, so we
        -- return False:
        else return False

    where isSomething = isJust -- Going for a more readable name here.

-- | A Turtle-friendly form of "getProgName":
getProgName' :: MonadIO io => io Line
getProgName' =
    -- "liftIO" converts "getProgName" from an "IO String" to a more flexible
    -- "io String", where lowercase "io" works with either Turtle's "Shell" or
    -- "IO".
    -- We convert the result from a "String" to a "Line", using an "unsafe"
    -- function: it crashes if given a String containing any newlines. But
    -- program names do not have newlines, so we are good.
    unsafeStringToLine <$> liftIO getProgName

-- | Just like Turtle's "export", but accepts a Turtle.FilePath for the value:
exportFilePath :: MonadIO io => Text -> FilePath -> io ()
exportFilePath name val = export name (filePathToText val)

-- | Adds the FilePath to the PATH environment variable:
addToPath :: MonadIO io => FilePath -> io ()
addToPath filePath = do
    Just path <- Turtle.need "PATH"

    -- Appending "pgPath" to the environment PATH var, using a colon separator.
    -- Though we are exporting file paths, "path" and "pgPath" have type "Text",
    -- hence the "export", as opposed to "exportFilePath".
    export "PATH" (format (s%":"%s) path filePath')

    where filePath' = filePathToText filePath

-- | Print a header for a step
step :: Line -> Shell ()
step msg = err ("## " <> msg)

-- | Prints given line to stderr on True, does nothing otherwise:
-- (Monoid constraint makes "no-op" the default value for "io ()".)
(!>) :: ( MonadIO io, Monoid (io()) ) => Bool -> Line -> io ()
(!>) bool line = bool ?<> err line -- Default value for io () is a no-op.

-- | Prints given line on True, does nothing otherwise:
-- (Monoid constraint makes "no-op" the default value for "io ()".)
(?>) :: ( MonadIO io, Monoid (io()) ) => Bool -> Line -> io ()
(?>) bool line = bool ?<> echo line -- Default value for io () is a no-op.


---------------------- For verification & error handling -----------------------

-- | Feedback from verification: "Passed" or failed because of a given reason.
-- A reason for failure is given as a list of Lines for use with "mapM_ err".
data VerificationReport = Passed | FailedBecause [Line] deriving (Eq, Show)

-- | This class lets "verificationReport" be flexible for the data type:
class Verifiable a where
    verificationReport :: a -> VerificationReport

allPassed :: [VerificationReport] -> Bool
allPassed = all (Passed ==) -- Point-free def & currying the equality function.
                            -- True iff all equal "Passed".

catVeriReports :: [VerificationReport] -> VerificationReport
catVeriReports reports =

    -- If all reports have passed verification ...
    -- (which is vacuously true in the case of an empty report list)
    if (allPassed reports) then

        -- ... output "Passed".
        Passed

    -- Otherwise, concat the failure reasons and apply the type constructor:
    else FailedBecause (mconcat failureReasons)

      where

          failureReasons = map preCat reports

          -- With concat'ing, adds failure reason to the list we're building:
           -- ("x" is a list of Lines, singleton empty Line gives us a spacer
           -- Line when we print the concat'ed report.)
          preCat (FailedBecause x) = x <> [""]
          -- Does not affect concatenation:
          preCat Passed = []
