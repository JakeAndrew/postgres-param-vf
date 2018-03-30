module AdditionsTo.Turtle.Shake (actsh) where

import Turtle
import Development.Shake

-- | Use a 'Shell a' as an 'Action ()'
-- Like how Turtle has its own "Shell" instead of "IO", Shake has "Action".
-- The below lets us do Turtle things in a Shake Action.
actsh :: Shell a -> Action ()
actsh = liftIO . sh
