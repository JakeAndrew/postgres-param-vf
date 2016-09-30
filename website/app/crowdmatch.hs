-- | crowdmatch
--
-- Calculates and records the monthly donation totals for patrons.
--
module Main where

import Import.NoFoundation

import Data.Ratio

main = putStrLn "Under construction"

honk :: MonadIO m => SqlPersistT m ()
honk = do
    pledges :: [Pledge] <- map entityVal <$> selectList [] []
    let projectValue = fromIntegral (length pledges)
    now <- liftIO getCurrentTime
    mapM_ (recordDonation (DonationTime now) projectValue) pledges

recordDonation :: MonadIO m => DonationTime -> Int32 -> Pledge -> SqlPersistT m ()
recordDonation now amt Pledge{..} = do
    insert_ (DonationHistory _pledgeUsr now amt)
    void (upsert (Wallet _pledgeUsr amt) [WalletBalance +=. amt])
-- Take the set of pledges, calculate what everyone owes and to whom, write
-- it out.
--
-- But there's only one project, so that's pretty straightforward.
--
-- I guess the tricky part is what to do next. Store the amount owed
-- immediately? Oh, store the transaction and add the amount to a thing?
-- Then the payout mech can just pull from the thing.
