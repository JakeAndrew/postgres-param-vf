{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The one-stop module for the Crowdmatch mechanism!
module Crowdmatch (
        -- * Interface with your model
          ToCrowdmatchPatron(..)
        , crowdmatchManualMigrations
        , migrateCrowdmatch
        , SqlRunner

        -- * Interface with stripe
        , StripeRunner
        , runStripe

        -- * Store/delete payment tokens
        , storePaymentToken
        , deletePaymentToken

        -- * Store/delete pledges
        , storePledge
        , deletePledge

        -- * Trigger a crowdmatch
        , crowdmatch

        -- * Trigger making payments
        , makePayments

        -- * Data retrieval
        , fetchProject
        , fetchPatron

        -- * Types returned by crowdmatch actions
        , Patron(..)
        , PatronId(..)
        , Project(..)
        , DonationUnits(..)
        , HistoryTime(..)
        , Cents(..)
        , PaymentToken(..)

        -- * Internal stuff, mostly for tests
        , CrowdmatchI(..)
        , runMech
        , StripeI(..)
        , PPtr(..)
        , donationCents
        ) where

import Control.Error (ExceptT(..), runExceptT)
import Control.Lens ((^.), from, view, Iso', iso)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Function (on)
import Data.Maybe
import Data.Ratio
import Data.Time (UTCTime, getCurrentTime, utctDay)
import Data.Time.Clock (getCurrentTime)
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import System.Environment
import System.IO
import Web.Stripe (stripe, (-&-), StripeConfig, Expandable(..))
import Web.Stripe.Balance
import Web.Stripe.Charge
import Web.Stripe.Customer
        ( TokenId
        , Customer
        , CustomerId
        , customerId
        , updateCustomer
        , createCustomer
        , deleteCustomer)
import Web.Stripe.Error (StripeError)
import qualified Data.ByteString.Char8 as BS

import Crowdmatch.Model hiding (Patron(..))
import qualified Crowdmatch.Model as Model
import qualified Crowdmatch.Skeleton as Skeleton

-- For doctests:
--
-- $setup
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary Cents where arbitrary = Cents . getPositive <$> arbitrary

-- | A method that runs 'SqlPersistT' values in your environment.
type SqlRunner io env = forall a. SqlPersistT io a -> env a

-- | A method that runs 'StripeI' instructions in IO. A default that uses
-- 'stripe' is provided by 'runStripe'.
type StripeRunner = forall io.
    MonadIO io => forall a. StripeI a -> io (Either StripeError a)

--
-- THE ACTUAL INTERFACE USED BY THE WEBSITE
--

-- | Information about a particular patron, returned by 'fetchPatron'.
data Patron = Patron
        { patronCreated :: UTCTime
        , patronPaymentToken :: Maybe PaymentToken
        , patronDonationPayable :: DonationUnits
        , patronPledgeSince :: Maybe UTCTime
        }
        deriving (Eq, Show)

-- | Data about a project. There's only one, Snowdrift, so this is rather
-- simple. Returned with 'fetchProject'.
data Project = Project
        { projectCrowd :: Int
        , projectMonthlyIncome :: Cents
        , projectPledgeValue :: DonationUnits
        , projectDonationReceivable :: DonationUnits
        }

-- | Record a 'TokenId' for a patron.
storePaymentToken
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env
    -> StripeRunner
    -> usr -- ^ your model's user, an instance of ToCrowdmatchPatron
    -> TokenId -- ^ you must independently get this from stripe
    -> env (Either StripeError ())
storePaymentToken db strp usr =
    runMech db . StorePaymentTokenI strp (usr ^. from external)

-- NB: The "-- ^" in the following methods is intentional. It forces
-- Haddocks to reformat the arguments in a pleasing way.

-- | Delete the 'TokenId'. This will remove any existing pledges, since a
-- a token is required for pledging.
deletePaymentToken
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> StripeRunner
    -> usr
    -> env (Either StripeError ())
deletePaymentToken db strp =
    runMech db . DeletePaymentTokenI strp . (^. from external)

-- | Stores a pledge, joining the crowd. Requires the patron to already
-- have a payment token available.
storePledge
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> usr
    -> env ()
storePledge db = runMech db . StorePledgeI . (^. from external)

-- | Delete a pledge, leaving the crowd.
deletePledge
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> usr
    -> env ()
deletePledge db = runMech db . DeletePledgeI . (^. from external)

-- | Retrieve info on the project.
fetchProject
    :: (MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> env Project
fetchProject db = runMech db FetchProjectI

-- | Retrieve info on a particular patron.
fetchPatron
    :: (ToCrowdmatchPatron usr, MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> usr
    -> env Patron
fetchPatron db = runMech db . FetchPatronI . (^. from external)

crowdmatch
    :: (MonadIO io, MonadIO env)
    => SqlRunner io env -- ^
    -> env ()
crowdmatch db = runMech db CrowdmatchI

--
-- ONE LEVEL DOWN
-- wherein we use our internal, Markov-able api
--

-- | Actions provided by the library
data CrowdmatchI return where
    StorePaymentTokenI
        :: StripeRunner
        -> PPtr
        -> TokenId
        -> CrowdmatchI (Either StripeError ())
    DeletePaymentTokenI
        :: StripeRunner
        -> PPtr
        -> CrowdmatchI (Either StripeError ())
    StorePledgeI :: PPtr -> CrowdmatchI ()
    DeletePledgeI :: PPtr -> CrowdmatchI ()
    FetchProjectI :: CrowdmatchI Project
    FetchPatronI :: PPtr -> CrowdmatchI Patron
    CrowdmatchI :: CrowdmatchI ()

-- | Executing the actions
runMech
    :: (MonadIO env, MonadIO io)
    => SqlRunner io env -> CrowdmatchI return -> env return

--
-- Payment token (store/delete)
--

runMech db (StorePaymentTokenI strp pptr cardToken) = do
    Entity pid p <- db (upsertPatron pptr [])
    runExceptT $ do
        ret <- ExceptT $ maybe create' update' (Model.patronPaymentToken p)
        ExceptT (Right <$> updatePatron' pid ret)
  where
    create' = stripeCreateCustomer strp cardToken
    update' = stripeUpdateCustomer strp cardToken . unPaymentToken
    updatePatron' pid c = do
        now <- liftIO getCurrentTime
        let payToken = PaymentToken (customerId c)
        db $ do
            _ <- insert (PaymentTokenHistory pid (HistoryTime now) Create)
            update pid [PatronPaymentToken =. Just payToken]

-- FIXME: Feedback on nonexisting CustomerId.
runMech db (DeletePaymentTokenI strp pptr) = do
    Entity pid p <- db (upsertPatron pptr [])
    maybe (pure (Right ())) (deleteToken' pid) (Model.patronPaymentToken p)
  where
    deleteToken' pid (PaymentToken cust) = do
        res <- stripeDeleteCustomer strp cust
        traverse (const (onStripeSuccess' pid)) res
    onStripeSuccess' pid = do
        now <- liftIO getCurrentTime
        -- Must delete pledges if there's no payment method!
        -- Fixme: Duplication of upsert
        runMech db (DeletePledgeI pptr)
        db $ do
            _ <- insert (PaymentTokenHistory pid (HistoryTime now) Delete)
            update pid [PatronPaymentToken =. Nothing]

--
-- Pledge (store/delete)
--

-- FIXME: Feedback on missing payment info
-- FIXME: Feedback on existing pledges
runMech db (StorePledgeI pptr) = do
    Entity pid p <- db (upsertPatron pptr [])
    maybe noCustomer (checkpledge pid) (pure p <* Model.patronPaymentToken p)
  where
    checkpledge pid p =
        maybe (pledge' pid) existingPledge (Model.patronPledgeSince p)
    pledge' pid = db $ do
        now <- liftIO getCurrentTime
        update pid [PatronPledgeSince =. Just now]
        insert_ (PledgeHistory pid now Create)
    noCustomer = pure ()
    existingPledge _ = pure ()

-- FIXME: Feedback on nonexistent pledge.
runMech db (DeletePledgeI pptr) = db $ do
    -- In the absence of triggers or other database use sophistication, we
    -- fetch/evaluate/modify here.
    Entity pid p <- upsertPatron pptr []
    maybe noPledge  (const (delete' pid)) (Model.patronPledgeSince p)
  where
    noPledge = pure ()
    delete' pid = do
        now <- liftIO getCurrentTime
        update pid [PatronPledgeSince =. Nothing]
        insert_ (PledgeHistory pid now Delete)

runMech db FetchProjectI = db $ do
    numPledges <- count [PatronPledgeSince !=. Nothing]
    -- Persistent terrible SQL :|
    receivable <-
        fmap
            (sum . map (Model.patronDonationPayable . entityVal))
            (selectList [] [])
    let pledgevalue = DonationUnits (fromIntegral numPledges)
        income = view donationCents (pledgevalue * pledgevalue)
    pure (Project numPledges income pledgevalue receivable)

runMech db (FetchPatronI pptr) =
    db $ fromModel . entityVal <$> upsertPatron pptr []

runMech db CrowdmatchI = db $ do
    active <- Skeleton.activePatrons
    let projectValue = fromIntegral (length active)
    today <- utctDay <$> liftIO getCurrentTime
    mapM_
        (recordCrowdmatch (CrowdmatchDay today) (DonationUnits projectValue))
        active
  where
    recordCrowdmatch day amt (Entity pid _) = do
        insert_ (CrowdmatchHistory pid day amt)
        void (update pid [PatronDonationPayable +=. amt])

--
-- I N C E P T I O N
--
-- (one more level down)
-- Wherein we abstract over the possible ways of running Stripe.
--

stripeCreateCustomer
    :: MonadIO io => StripeRunner -> TokenId -> io (Either StripeError Customer)
stripeCreateCustomer strp = liftIO . strp . CreateCustomerI

stripeUpdateCustomer
    :: MonadIO io
    => StripeRunner
    -> TokenId
    -> CustomerId
    -> io (Either StripeError Customer)
stripeUpdateCustomer strp tok = liftIO . strp . UpdateCustomerI tok

stripeDeleteCustomer
    :: MonadIO io
    => StripeRunner
    -> CustomerId
    -> io (Either StripeError ())
stripeDeleteCustomer strp = liftIO . strp . DeleteCustomerI

-- | Stripe instructions we use
data StripeI a where
    CreateCustomerI :: TokenId -> StripeI Customer
    UpdateCustomerI :: TokenId -> CustomerId -> StripeI Customer
    DeleteCustomerI :: CustomerId -> StripeI ()

-- | A default stripe runner
runStripe
    :: MonadIO io
    => StripeConfig -> StripeI a -> io (Either StripeError a)
runStripe c = \case
    CreateCustomerI cardToken ->
        liftIO (stripe c (createCustomer -&- cardToken))
    UpdateCustomerI cardToken cust ->
        liftIO (stripe c (updateCustomer cust -&- cardToken))
    DeleteCustomerI cust ->
        void <$> liftIO (stripe c (deleteCustomer cust))


--
-- Making payments
--

-- | Stripe measures charges in cents. Handy!
chargeCents :: Iso' Cents Amount
chargeCents = iso toAmount fromAmount
  where
    toAmount (Cents i) = Amount (fromIntegral i)
    fromAmount (Amount i) = Cents (fromIntegral i)

type StripeResult = Either StripeError Charge

data ChargeResult = ChargeResult
        { _chargeResultPatron :: PatronId
        , _chargeResultFee :: Cents
        , _chargeResultNet :: DonationUnits
        , _chargeResultStripe :: StripeResult
        } deriving (Show)

-- | Calculate Stripe's fee: 2.9% + 30¢
--
-- https://stripe.com/us/pricing
--
-- Stripe uses financial rounding, aka the rounding everyone outside the US
-- learns (apparently). This is the rounding implemented in Prelude, as
-- well. Hooray!
--
-- If we ever have integration testing, we should confirm the following
-- holds true:
--
--      $5.00 charge -> 44.5¢ fee -> Stripe rounded to 44¢
--     $15.00 charge -> 73.5¢ fee -> Stripe rounded to 74¢
--
-- I confirmed these facts when I wrote this function, but tests ftw.
stripeFee :: Cents -> Cents
stripeFee = round . (+ 30) . (* 0.029) . fromIntegral

-- | As of 2016-10-10, the amount a patron pays is increased so that the
-- amount the project receives is equal to the amount they crowdmatched.
--
-- Proving that the rounding always works out was annoying, but I did it
-- with a brute-force program. It's ok up until integer underflows around
-- ~$20M.
--
-- prop> \d -> d < 2*10^9 ==> let {p = payment d; f = stripeFee p} in p-f==d
payment :: Cents -> Cents
payment = round . (/ (1 - 0.029)) . (+ 30) . fromIntegral

-- | A donation is sufficient for processing if the Stripe fee is < 10%.
-- https://tree.taiga.io/project/snowdrift/issue/457
--
-- This function is useful for testing, but we memoize its
-- production-required result below.
--
-- Since we're using the 'payment' function right now, this equation is
-- different from the long term ideal.
sufficientDonation :: DonationUnits -> Bool
sufficientDonation d =
    fee % p < maximumFee
  where
    p = payment (d^.donationCents)
    fee = stripeFee p
    maximumFee = ((%) `on` Cents) 1 10

-- | This is the minimum amount that satisfies 'sufficientDonation'. You can
-- find it for yourself by running:
-- >>> :{
-- >>> let x = head . filter (\x -> all sufficientDonation [x..x+35])
-- >>>              . map DonationUnits
-- >>>              $ [10..]
-- >>> in (x, x == minimumDonation)
-- >>>:}
-- (DonationUnits 3790,True)
--
-- Note that rounding makes the function discontinuous, with a step every
-- 1/0.029 ~ 35 DonationUnits. There's a local optimum at ~3610, but we'll just
-- skip that one, cause that's weird.
--
-- Since we're using the 'payment' function right now, this value is higher
-- than the long term ideal.
minimumDonation :: DonationUnits
minimumDonation = DonationUnits 3790

-- | The projection of a Patron that can, and should, make a donation.
data Donor = Donor
        { _donorPatron :: PatronId
        , _donorCustomer :: CustomerId
        , _donorDonationPayable :: DonationUnits
        } deriving (Show)

-- | Send charge commands to Stripe.
--
-- This holds a lock on the database to ensure consistency. That could kill
-- concurrent performance, but right now the only thing hitting the payment
-- tables is this utility and the crowdmatch utility. None of those should ever
-- be run simultaneously at present, so I'd rather have bad "performance" on
-- operational mistakes, rather than bad/duplicate charges. :)
makePayments :: MonadIO m => StripeConfig -> SqlPersistT m ()
makePayments conf = do
    -- Duplicating sql logic with Haskell logic to get rid of patrons
    -- without a CustomerId :/
    --
    -- #1 (hidden because Esqueleto)
    chargeable <- Skeleton.patronsReceivable minimumDonation
    let donors =
            -- #2
            mapMaybe
                (\(Entity pId p) ->
                    Donor
                    <$> Just pId
                    <*> fmap unPaymentToken (Model.patronPaymentToken p)
                    <*> Just (Model.patronDonationPayable p))
                chargeable
    chargeResults <- liftIO (traverse (sendCharge conf) donors)
    mapM_ (recordResults conf) chargeResults

-- | Send the charge command to Stripe
--
-- For the Futurama milestone, we tack on a fee that covers the Stripe fee
-- to calculate the 'payment'.
sendCharge
    :: StripeConfig
    -> Donor
    -> IO ChargeResult
sendCharge conf Donor{..} =
    (ChargeResult _donorPatron fee net <$>)
        . stripe conf
        . (-&- _donorCustomer)
        -- Supported upstream as of 2016-10-06, but not in our resolver yet
        -- . (-&- ExpandParams ["balance_transaction"])
        . flip createCharge USD
        . view chargeCents
        $ cents
  where
    cents = payment (_donorDonationPayable ^. donationCents)
    fee = stripeFee cents
    net = (cents - fee) ^. from donationCents

recordResults
    :: MonadIO m
    => StripeConfig
    -> ChargeResult
    -> SqlPersistT m ()
recordResults conf res@ChargeResult {..} =
    either
        (const (liftIO (hPrint stderr res)))
        recordDonation
        _chargeResultStripe
  where
    recordDonation c@Charge {..} = do
        ts <- liftIO (donationTimestamp conf c)
        insert_
            (DonationHistory
                _chargeResultPatron
                ts
                _chargeResultNet
                _chargeResultFee)
        update _chargeResultPatron [PatronDonationPayable -=. _chargeResultNet]

-- | Tries to get the timestamp from the Charge's TransactionBalance
-- sub-item. If that fails, it's cool, we'll just use a local variant of
-- "now".
--
-- I don't want to bail on recording the charge if we can't get the
-- timestamp, since the presence of the Charge itself means Stripe
-- processed it. There was merely a secondary failure getting the
-- TransactionBalance. Ideally we'd retry, with some sort of 'pending'
-- status, but let's slap that together later.
donationTimestamp :: StripeConfig -> Charge -> IO HistoryTime
donationTimestamp conf = fmap HistoryTime . chargeTime
  where
    fallback = getCurrentTime
    chargeTime Charge {..} =
        maybe fallback transactionTime chargeBalanceTransaction
    transactionTime = \case
        Expanded BalanceTransaction {..} -> pure balanceTransactionCreated
        Id balId -> (=<<)
            (either (const fallback) (pure . balanceTransactionCreated))
            (stripe conf (getBalanceTransaction balId))

--
-- Helpers
--

-- | Haskell doesn't know it, but a PPtr should always be linked to a
-- Patron. This function ensures it in Haskell-land.
--
-- (Also, creating the proper database constraint is still TODO, so we
-- actually need this code.)
upsertPatron
    :: MonadIO m
    => PPtr
    -> [Update Model.Patron]
    -> SqlPersistT m (Entity Model.Patron)
upsertPatron pptr mods = do
    now <- liftIO getCurrentTime
    upsert (Model.Patron pptr now Nothing 0 Nothing) mods

fromModel :: Model.Patron -> Patron
fromModel (Model.Patron _usr t c d p) = Patron t c d p

-- | DonationUnits are truncated to usable cents for use in creating
-- charges.
donationCents :: Iso' DonationUnits Cents
donationCents = iso toCents fromCents
  where
    fromCents = fromIntegral . (* 10)
    toCents = fromIntegral . (`div` 10)
