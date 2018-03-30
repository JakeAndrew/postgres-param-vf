module AdditionsTo.Turtle.Shell (hush) where

import Control.Exception.Base (bracket)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO (openFile, IOMode(..))
import qualified System.IO as H

import Turtle


-- | Run a shell and send stdout/stderr to nowhere
hush :: Shell () -> Shell ()
hush act = liftIO $ bracket
    (do
        saveErr <- hDuplicate H.stderr
        saveOut <- hDuplicate H.stdout
        h <- openFile "/dev/null" WriteMode
        hDuplicateTo h H.stderr
        hDuplicateTo h H.stdout
        return (saveErr, saveOut)
    )
    (\(saveErr, saveOut) -> do
        hDuplicateTo saveErr H.stderr
        hDuplicateTo saveOut H.stdout)
    (const (sh act))
