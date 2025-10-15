{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           , RecordWildCards
           , StandaloneDeriving
           #-}

module Data.IERS where

import Codec.Serialise

import Control.Applicative

import Control.Monad

import qualified Data.ByteString.Char8 as BC

import qualified Data.Attoparsec.ByteString.Char8 as A

import Data.Functor

import qualified Data.IntMap.Strict as IM

import Data.Time.Calendar
-- import Data.Time.Clock

import GHC.Generics

import Network.HTTP.Req

dayToKey :: Day -> IM.Key
dayToKey (ModifiedJulianDay mjd) = fromIntegral mjd

keyToDay :: IM.Key -> Day
keyToDay k = ModifiedJulianDay (fromIntegral k)

insByDay :: (a -> Day) -> a -> IM.IntMap a -> IM.IntMap a
insByDay f x = IM.insert (dayToKey (f x)) x

fromListByDay :: (a -> Day) -> [a] -> IM.IntMap a
fromListByDay f = IM.fromList . fmap (\x -> (dayToKey (f x), x))

-- Orphans
deriving newtype instance Serialise Day

data DUT1 = DUT1 {
    dut1ValidFrom :: Day
    -- | UT1 - UTC in increments of 0.1s
  , dut1UT1MinusUTCDeciS :: Int
  } deriving stock ( Generic
                   , Eq
                   , Show
                   )
    deriving anyclass Serialise

data CurrentLeapSeconds = CurrentLeapSeconds {
    clsValidFrom :: Day
    -- | TAI - UTC in whole seconds.
  , clsTAIMinusUTC :: Int
  } deriving stock ( Generic
                   , Eq
                   , Show
                   )
    deriving anyclass Serialise

data RapidEOP = RapidEOP {
    reopDay :: Day
    -- | arcseconds
  , reopX :: Double
    -- | arcseconds
  , reopXError :: Double
    -- | arcseconds
  , reopY :: Double
    -- | arcseconds
  , reopYError :: Double
    -- | UT1 - UTC in seconds
  , reopUT1MinusUTC :: Double
    -- | seconds
  , reopUT1MinusUTCError :: Double
  } deriving stock ( Generic
                   , Eq
                   , Show
                   )
    deriving anyclass Serialise

data PredictedEOP = PredictedEOP {
    peopDay :: Day
    -- | arcseconds
  , peopX :: Double
    -- | arcseconds
  , peopY :: Double
  , peopUT1MinusUTC :: Double
  } deriving stock ( Generic
                   , Eq
                   , Show
                   )
    deriving anyclass Serialise

data NEOS = NEOS {
    neosDay :: Day
    -- | milliseconds of arc
  , neosDPsi :: Double
    -- | milliseconds of arc
  , neosDPsiError :: Double
    -- | milliseconds of arc
  , neosDEpsilon :: Double
    -- | milliseconds of arc
  , neosDEpsilonError :: Double
  } deriving stock ( Generic
                   , Eq
                   , Show
                   )
    deriving anyclass Serialise

data IAU = IAU {
    iauDay :: Day
    -- | milliseconds of arc
  , iauDX :: Double
    -- | milliseconds of arc
  , iauDXError :: Double
    -- | milliseconds of arc
  , iauDY :: Double
    -- | milliseconds of arc
  , iauDYError :: Double
  } deriving stock ( Generic
                   , Eq
                   , Show
                   )
    deriving anyclass Serialise

data BulletinA = BulletinA {
    baPublishedDate :: Day
  , baVol :: String
  , baNumber :: Int
  , baDUT1 :: IM.IntMap DUT1
  , baLeapSeconds :: CurrentLeapSeconds
  , baREOP :: IM.IntMap RapidEOP
  , baPEOP :: IM.IntMap PredictedEOP
    -- | Only in newer reports.
  , baNEOS :: IM.IntMap NEOS
    -- | Only in newer reports.
  , baIAU :: IM.IntMap IAU
  } deriving stock ( Generic
                   , Eq
                   , Show
                   )
    deriving anyclass Serialise

type BulletinASet = IM.IntMap BulletinA

data EOPQueryResult = EOPQueryResult {
    eqrTime :: Day
    -- | arcseconds
  , eqrX :: Double
    -- | arcseconds
  , eqrY :: Double
    -- | UT1 - UTC in seconds
  , eqrDUT :: Double
  } 

queryREOP :: RapidEOP -> EOPQueryResult
queryREOP RapidEOP{..} =
    let eqrTime = reopDay
        eqrX = reopX
        eqrY = reopY
        eqrDUT = reopUT1MinusUTC
    in EOPQueryResult{..}

queryPEOP :: PredictedEOP -> EOPQueryResult
queryPEOP PredictedEOP{..} =
    let eqrTime = peopDay
        eqrX = peopX
        eqrY = peopY
        eqrDUT = peopUT1MinusUTC
    in EOPQueryResult{..}

-- We make a few assumptions here:
-- - Data from more recent bulletins is better than data from older ones.
-- - REOPs are better than PEOPs
 
mostRecentEOP :: BulletinASet -> Maybe EOPQueryResult
mostRecentEOP bas = do
    BulletinA{..} <- snd <$> IM.lookupMax bas
    queryREOP . snd <$> IM.lookupMax baREOP

queryEOP :: BulletinASet -> Day -> Either String EOPQueryResult
queryEOP bas d =
    let e s = maybe (Left s) Right
        k = dayToKey d
        (ltb, eb, gtb) = IM.splitLookup k bas
    in case eb of
        Just BulletinA{..} ->
            case IM.lookup k baREOP of
                Just r -> pure (queryREOP r)
                -- This is not supposed to happen. There are no bulletins in the
                -- test data set that don't have their publication date in the
                -- REOP table.
                Nothing ->
                    Left "REOP table did not contain its own publication date"
        Nothing
            -- day is after most recently published, so check REOPs then PEOPs
            | IM.null gtb && not (IM.null ltb) -> do
                BulletinA{..} <- snd <$> e "absurd" (IM.lookupMax ltb)
                e "query beyond prediction range" $
                    (queryREOP <$> IM.lookup k baREOP)
                    <|> (queryPEOP <$> IM.lookup k baPEOP)
            | IM.null gtb && IM.null ltb -> Left "table was empty"
            | not (IM.null gtb) -> do
                BulletinA{..} <- snd <$> e "absurd" (IM.lookupMin gtb)
                e "REOP table lacked expected time range coverage" $
                    queryREOP <$> IM.lookup k baREOP
            | otherwise -> Left "absurd"

char_ :: Char -> A.Parser ()
char_ = void . A.char

string_ :: BC.ByteString -> A.Parser ()
string_ = void . A.string

decimal_ :: A.Parser ()
decimal_ = void A.decimal

double :: A.Parser Double
double = A.signed (dub <|> A.double)
    where dub = do
            string_ "."
            ibs' <- A.takeWhile1 A.isDigit
            let ibs = BC.dropWhileEnd (== '0') ibs'
            i <- case BC.readInt ibs of
                    Nothing -> fail "absurd"
                    Just (x, bs) | BC.null bs -> pure x
                                 | otherwise -> fail "absurd"
            let pv = BC.length (BC.dropWhileEnd (== '0') ibs)
            pure (fromIntegral i / fromIntegral (10 ^ pv))

skipLine :: A.Parser ()
skipLine = void $ A.skipWhile (/= '\n') *> A.char '\n'

skipLines :: A.Parser ()
skipLines = A.skipMany1 skipLine

skipLinesUntil :: A.Parser a -> A.Parser a
skipLinesUntil p = p <|> (skipLine *> skipLinesUntil p) 

space1_ :: A.Parser ()
space1_ = void $ A.takeWhile1 A.isSpace

decimalInRange :: (Show a, Integral a)
               => String
               -> a
               -> a
               -> A.Parser a
decimalInRange err mn mx = do
    i <- A.decimal
    if i >= mn && i <= mx
    then pure i
    else fail ("No such " <> err <> ": " <> show i)

parseDayOfMonth :: A.Parser DayOfMonth
parseDayOfMonth = decimalInRange "day of month" 1 31

parseMonthOfYear :: A.Parser MonthOfYear
parseMonthOfYear = A.choice
  [ A.string "January" $> 1
  , A.string "February" $> 2
  , A.string "March" $> 3
  , A.string "April" $> 4
  , A.string "May" $> 5
  , A.string "June" $> 6
  , A.string "July" $> 7
  , A.string "August" $> 8
  , A.string "September" $> 9
  , A.string "October" $> 10
  , A.string "November" $> 11
  , A.string "December" $> 12
  , A.string "Jan" $> 1
  , A.string "Feb" $> 2
  , A.string "Mar" $> 3
  , A.string "Apr" $> 4
  , A.string "Jun" $> 6
  , A.string "Jul" $> 7
  , A.string "Aug" $> 8
  , A.string "Sept" $> 9
  , A.string "Sep" $> 9
  , A.string "Oct" $> 10
  , A.string "Nov" $> 11
  , A.string "Dec" $> 12
  ]

parseYear :: A.Parser Year
parseYear = A.decimal

parseGregorianDate :: A.Parser Day
parseGregorianDate = do
    dom <- parseDayOfMonth 
    space1_ 
    moy <- parseMonthOfYear 
    space1_ 
    y <- parseYear 
    case fromGregorianValid y moy dom of
        Nothing -> fail ("Invalid Gregorian date " <> show (y, moy, dom))
        Just d -> pure d

parseMJD :: A.Parser Day
parseMJD = ModifiedJulianDay <$> A.decimal

parseVol :: A.Parser String
parseVol = A.string "Vol."
        *> space1_
        *> (BC.unpack <$> A.takeTill A.isSpace)

parseNumber :: A.Parser Int
parseNumber = A.string "No."
           *> space1_
           *> A.decimal

parseSign :: Num a => A.Parser (a -> a)
parseSign = char_ '+' $> id
        <|> char_ '-' $> negate

parseDUT1Val :: Integral a => A.Parser a
parseDUT1Val = zero <|> nonZero
    where zero = string_ "0.0" $> 0
          nonZero = do
            sign <- parseSign 
            string_ "0." 
            sign <$> A.decimal 

parseDUT1s :: A.Parser (IM.IntMap DUT1)
parseDUT1s = fromListByDay dut1ValidFrom <$> A.many1 parseDUT1

parseDUT1 :: A.Parser DUT1
parseDUT1 = do
    space1_
    string_ "="
    space1_
    dut1UT1MinusUTCDeciS <- parseDUT1Val 
    string_ " seconds beginning " 
    dut1ValidFrom <- parseGregorianDate 
    skipLine 
    pure DUT1{..}

seekToDUT1Header :: A.Parser ()
seekToDUT1Header = do
    space1_
    string_ "DUT1="
    skipLine

parseCurrentLeapSeconds :: A.Parser CurrentLeapSeconds
parseCurrentLeapSeconds = do
    space1_
    string_ "Beginning"
    space1_
    clsValidFrom <- parseGregorianDate
    skipLine
    space1_
    string_ "TAI-UTC"
    A.skipWhile (/= '=')
    string_ "="
    space1_
    clsTAIMinusUTC <- A.decimal
    skipLine
    pure CurrentLeapSeconds{..}
    
parseREOPs :: A.Parser (IM.IntMap RapidEOP)
parseREOPs = fromListByDay reopDay <$> A.many1 parseREOP

parseREOP :: A.Parser RapidEOP
parseREOP = do
    space1_
    decimal_
    space1_
    decimal_
    space1_
    decimal_
    space1_
    reopDay <- parseMJD
    space1_
    reopX <- double
    space1_
    reopXError <- double
    space1_
    reopY <- double
    space1_
    reopYError <- double
    space1_
    reopUT1MinusUTC <- double
    space1_
    reopUT1MinusUTCError <- double
    skipLine
    pure RapidEOP{..}

parsePEOPs :: A.Parser (IM.IntMap PredictedEOP)
parsePEOPs = fromListByDay peopDay <$> A.many1 parsePEOP

parsePEOP :: A.Parser PredictedEOP
parsePEOP = do
    space1_
    decimal_
    space1_
    decimal_
    space1_
    decimal_
    space1_
    peopDay <- parseMJD
    space1_
    peopX <- double
    space1_
    peopY <- double
    space1_
    peopUT1MinusUTC <- double
    skipLine
    pure PredictedEOP{..}

parseNEOSs :: A.Parser (IM.IntMap NEOS)
parseNEOSs = fromListByDay neosDay <$> A.many1 parseNEOS

parseNEOS :: A.Parser NEOS
parseNEOS = do
    space1_
    neosDay <- parseMJD
    space1_
    neosDPsi <- double
    space1_
    neosDPsiError <- double
    space1_
    neosDEpsilon <- double
    space1_
    neosDEpsilonError <- double
    skipLine
    pure NEOS{..}
    
parseIAUs :: A.Parser (IM.IntMap IAU)
parseIAUs = fromListByDay iauDay <$> A.many1 parseIAU

parseIAU :: A.Parser IAU
parseIAU = do
    space1_
    iauDay <- parseMJD
    space1_
    iauDX <- double
    space1_
    iauDXError <- double
    space1_
    iauDY <- double
    space1_
    iauDYError <- double
    skipLine
    pure IAU{..}

seekToREOPHeader :: A.Parser ()
seekToREOPHeader = do
    space1_
    string_ "COMBINED EARTH ORIENTATION PARAMETERS:"
    skipLine
    skipLine
    space1_
    string_ "IERS Rapid Service"
    skipLine
    space1_
    string_ "MJD"
    space1_
    string_ "x"
    space1_
    string_ "error"
    space1_
    string_ "y"
    space1_
    string_ "error"
    space1_
    string_ "UT1-UTC"
    space1_
    string_ "error"
    skipLine
    space1_
    char_ '"'
    space1_
    char_ '"'
    space1_
    char_ '"'
    space1_
    char_ '"'
    space1_
    char_ 's'
    space1_
    char_ 's'
    skipLine

seekToPEOPHeader :: A.Parser ()
seekToPEOPHeader = do
    space1_
    string_ "MJD"
    space1_
    string_ "x(arcsec)"
    space1_
    string_ "y(arcsec)"
    space1_
    string_ "UT1-UTC(sec)"
    skipLine

seekToNEOSHeader :: A.Parser ()
seekToNEOSHeader = do
    space1_
    string_ "MJD"
    space1_
    string_ "dpsi"
    space1_
    string_ "error"
    space1_
    string_ "deps"
    space1_
    string_ "error"
    skipLine
    space1_
    string_ "(msec. of arc)"
    skipLine

seekToIAUHeader :: A.Parser ()
seekToIAUHeader = do
    space1_
    string_ "MJD"
    space1_
    string_ "dX"
    space1_
    string_ "error"
    space1_
    string_ "dY"
    space1_
    string_ "error"
    skipLine
    space1_
    string_ "(msec. of arc)"
    skipLine

seekAndParseNEOSs :: A.Parser (IM.IntMap NEOS)
seekAndParseNEOSs = have <|> haveNot
    where have = do
            skipLinesUntil seekToNEOSHeader
            parseNEOSs
          haveNot = pure mempty

seekAndParseIAUs :: A.Parser (IM.IntMap IAU)
seekAndParseIAUs = have <|> haveNot
    where have = do
            skipLinesUntil seekToIAUHeader
            parseIAUs
          haveNot = pure mempty

parseBulletinA :: A.Parser BulletinA
parseBulletinA = do
    baPublishedDate <- skipLinesUntil (space1_ *> parseGregorianDate) 
    space1_
    baVol <- parseVol 
    space1_
    baNumber <- parseNumber 
    skipLine
    skipLinesUntil seekToDUT1Header
    baDUT1 <- parseDUT1s
    -- There are no lines between DUT1 and leap seconds
    baLeapSeconds <- parseCurrentLeapSeconds
    skipLinesUntil seekToREOPHeader 
    baREOP <- parseREOPs 
    skipLinesUntil seekToPEOPHeader 
    baPEOP <- parsePEOPs 
    baNEOS <- seekAndParseNEOSs
    baIAU <- seekAndParseIAUs
    A.skipMany skipLine 
    A.endOfInput
    pure BulletinA{..}

fetchBulletinA :: IO BulletinA
fetchBulletinA = do
    response <- runReq defaultHttpConfig $ req
      GET
      (https "datacenter.iers.org"
          /: "data"
          /: "latestVersion"
          /: "bulletinA.txt"
      )
      NoReqBody
      bsResponse
      mempty
    let bs = responseBody response
    case A.parseOnly parseBulletinA bs of
        Left e -> fail e
        Right b -> pure b
