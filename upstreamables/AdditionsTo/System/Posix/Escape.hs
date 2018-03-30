module AdditionsTo.System.Posix.Escape (escapeText) where

import Data.Text as Txt
import System.Posix.Escape
import Flow

-- | Just like System.Posix.Escape's "escape", but takes and gives "Text".
escapeText :: Text -> Text
escapeText = Txt.unpack .> escape .> Txt.pack
