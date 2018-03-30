{-# LANGUAGE OverloadedStrings #-}

-- NB: It appears that there is no good way to generalize "Shell a" values into
-- "MonadIO io => io a" values. That's probably by design and probably a good
-- thing. Please see "stackPathGetProjRoot". It is a "Shell FilePath". This
-- propagates the "Shell" type. That propagation ends in "maybePassProjRoot",
-- where we finally come to an "io ()" type, having passed the "FilePath" inside
-- the "Shell" on to the thing that needs it.

-- Currently, the "stack" package on Hackage is "only intended for use by the
-- executable." So instead of using an "upstream" API to Stack's features, we
-- interface with the executable via Turtle. The below module augments the
-- Stack.Path module in the "stack" package:
module AdditionsTo.Stack.Path.MaybeGetProjRoot
    (
      maybePassProjRoot
    , maybeGetProjRoot
    , ProjRootErrorInfo ( ProjRootErrorInfo
                        , execNotFound
                        , cantFindProjRootError
                        , projRootIndicator
                        )
    )
    where

-- Using Turtle's FilePath, not Prelude's:
import Prelude hiding (FilePath)

import Data.Text hiding (empty) -- Again, using Turtle's.
import Turtle

import AdditionsTo.Turtle

-- Instead of hard-coding our error messages into this module, we abstract them
-- out, so, in theory, this module could be sent upstream to the "stack"
-- package. Now that we have to pass our error messages around, we use this
-- named-field tuple so that we only have to pass one parameter instead of two,
-- and to make the code more readable.
data ProjRootErrorInfo = ProjRootErrorInfo
    {
      -- Action that prints an error for when the "stack" executable
      -- cannot be found:
      -- NB: Cannot use class contraint in the type of a field in a tuple.
      -- So this cannot be "MonadIO io => io ()".
      execNotFound :: Shell ()

      -- Action that prints an error for when the reported project root does
      -- not seem right. This happens when `stack path --project-root` is ran
      -- outside of any Stack project dir, which gives you a default answer,
      -- which, of course, is not the dir we're looking for.
      -- It accepts a FilePath so that the reported root may be included in
      -- the error message.
    , cantFindProjRootError ::  FilePath -> Shell ()

    -- The indicator file we use to tell whether the reported project root
    -- is actually the path to our project.
    , projRootIndicator :: FilePath
    }

-- First, this obtains the project root path, or errors trying. On failure, a
-- supplied error message is printed. On success, it passes the project root's
-- path on to the supplied function.
maybePassProjRoot :: MonadIO io  => ProjRootErrorInfo -> (FilePath -> IO ()) -> io ()
maybePassProjRoot pREI passItHere = sh $ do

    -- Getting the project root is fallible, so the below action needs the
    -- error info tuple "pREI" to know what to say upon failure.
    -- "using" converts the "Shell (Maybe FilePath)" value into an
    -- "io (Maybe FilePath)" value.
    maybeProjRoot <- maybeGetProjRoot pREI

    -- So we have two cases to deal with:
    case maybeProjRoot of

        -- If "maybeProjRoot" is "Just" the directory path, pass it on to
        -- the supplied function and give back its "io a":
        Just projRoot -> liftIO $ passItHere projRoot

        -- If it's "Nothing" instead, we simply return the "unit" value.
        -- It's that simple because "maybeGetProjRoot" prints its own errors,
        -- so we don't have to do any of that here.
        Nothing -> return ()

-- | Action that hopefully returns the project root's path.
-- Returns "Nothing" upon failure, printing its own errors.
maybeGetProjRoot :: ProjRootErrorInfo -> Shell (Maybe FilePath)
maybeGetProjRoot pREI = do

    -- We'll be using the `stack path --project-root` command to get the project
    -- root. So we need to make sure the "stack" executable is available:
    haveStack <- testexec "stack"

    -- If we have the "stack" executable available to us ...
    if haveStack

        -- ... then we move on to part 2, passing our input along, and then we give
        -- back its result:
        then maybeGetPojectRootPart2 pREI

        -- Otherwise, we must print an error and then return Nothing:
        else execNotFound' >> (return Nothing)

      -- Get field out of pREI and then generalize it to the "io" type:
      where execNotFound' = sh (execNotFound pREI)


-- | Used by maybeGetProjectRoot. Same behavior except it assumes "stack" exists.
maybeGetPojectRootPart2 :: ProjRootErrorInfo -> Shell (Maybe FilePath)
maybeGetPojectRootPart2 pREI = do

    -- We assume that the "stack" executable is available because we already
    -- checked for it in maybeGetProjRoot.

    -- We now get the project root, as reported by the
    -- `stack path --project-root` command.
    -- NB: If this program is ran from outside the project root, the path
    -- obtained by this command will not be accurate, hence the upcoming check.
    reportedProjRoot <- stackPathGetProjRoot

    -- In the reported project root, does the indicator file exist? If it does
    -- not, we suspect that we got the wrong path from Stack.
    seemsRight <- testfile ( reportedProjRoot </> indicator )

    -- If the reported project root seems right ...
    if seemsRight

        -- ... then we return "Just" that:
        then return $ Just reportedProjRoot

        -- Otherwise, we must print an error and then return Nothing:
        else (cantFindItError reportedProjRoot) >> (return Nothing)

      where indicator = projRootIndicator pREI

            -- Get field out of pREI and then generalize it to the "io" type:
            cantFindItError = sh . (cantFindProjRootError pREI)


-- | Haskell wrapper for the `stack path --project-root` command:
stackPathGetProjRoot :: Shell FilePath
stackPathGetProjRoot =
    -- "inproc" gives a "Shell Line". "(fromText . lineToText)" converts a Line
    -- to Text and then into a "FilePath". Together with "<$>", we convert a
    -- "Shell Line" into a "Shell FilePath".
    (fromText . lineToText) <$>
        (inproc "stack" ["path", "--project-root"] empty)
