-- | This is the closet, where we keep the skeletons.
--
-- Esqueleto produces the most wicked type errors when things go awry.
-- Spooky! But given the alternatives of using rawSql (and thus losing the
-- best of type safety when building sql), or using plain ol' persistent
-- and doing all the relational logic in Haskell (how depressing), I will
-- reluctantly stick with Esqueleto. Its use will merely be sequestered
-- in this module (alternate name: Model.Sequestro!)
module Model.Skeleton where

import Import hiding (groupBy)

import Control.Lens ((%~), _1, _2)
import Database.Esqueleto

{-# ANN module ("HLint: ignore Redundant bracket" :: String) #-}

projectDonationHistory :: MonadIO m => SqlPersistT m [(DonationTime, Int)]
projectDonationHistory =
    fmap (map ((_1 %~ unValue) . (_2 %~ fromMaybe 0 . unValue)))
         donationHistoryQ
  where
    donationHistoryQ =
        select $
            from $ \dh -> do
                groupBy (time dh)
                orderBy [asc (time dh)]
                pure (time dh, total dh)
    time = (^. DonationHistoryTime)
    total = sum_ . (^. DonationHistoryAmount)
