{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Yahoo
  ( fetchTicker,
    showYahooResponse,
    showTickerInfo,
    getCloseOverTime,
    showTicks,
  )
where

import Control.Lens
import Data.Aeson
import Data.Char (toLower)
import GHC.Generics
import Network.Wreq
import Text.Printf

data YahooResponse = YahooResponse {chart :: Chart} deriving (Show, Generic)

data Chart = Chart
  { result :: [ResultSimple],
    error' :: Maybe String
  }
  deriving (Show, Generic)

data ResultSimple = ResultSimple
  { meta :: Yahoo.Meta,
    timestamp :: [Int],
    indicators :: Indicators
  }
  deriving (Show, Generic)

data Meta = Meta
  { currency :: String,
    symbol :: String,
    exchangeName :: String,
    fullExchangeName :: String,
    instrumentType :: String,
    firstTradeDate :: Int,
    regularMarketTime :: Int,
    hasPrePostMarketData :: Bool,
    gmtoffset :: Int,
    timezone :: String,
    exchangeTimezoneName :: String,
    regularMarketPrice :: Double,
    fiftyTwoWeekHigh :: Double,
    fiftyTwoWeekLow :: Double,
    regularMarketDayHigh :: Double,
    regularMarketDayLow :: Double,
    regularMarketVolume :: Int,
    longName :: String,
    shortName :: String,
    chartPreviousClose :: Double,
    previousClose :: Double,
    scale :: Int,
    priceHint :: Int,
    currentTradingPeriod :: CurrentTradingPeriod,
    tradingPeriods :: [[TradingPeriod]],
    dataGranularity :: String,
    range :: String,
    validRanges :: [String]
  }
  deriving (Show, Generic)

data CurrentTradingPeriod = CurrentTradingPeriod
  { pre :: TradingPeriod,
    regular :: TradingPeriod,
    post :: TradingPeriod
  }
  deriving (Show, Generic)

data TradingPeriod = TradingPeriod
  { tpTimezone :: String,
    tpStart :: Int,
    tpEnd :: Int,
    tpGmtoffset :: Int
  }
  deriving (Show, Generic)

data Indicators = Indicators
  { quote :: [Quote]
  }
  deriving (Show, Generic)

data Quote = Quote
  { low :: [Maybe Double],
    high :: [Maybe Double],
    volume :: [Maybe Int],
    close :: [Maybe Double],
    open :: [Maybe Double]
  }
  deriving (Show, Generic)

type Tick = (Int, Maybe Double)

instance FromJSON YahooResponse

instance FromJSON Chart

instance FromJSON Yahoo.Meta

instance FromJSON CurrentTradingPeriod

instance FromJSON TradingPeriod where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = dropTpLower}

instance FromJSON ResultSimple

instance FromJSON Indicators

instance FromJSON Quote

dropTpLower :: String -> String
dropTpLower s = case dropped of
  [] -> []
  (x : xs) -> toLower x : xs
  where
    dropped = drop 2 s

fetchTicker :: String -> IO (Either String YahooResponse)
fetchTicker ticker = do
  response <- get $ "https://query2.finance.yahoo.com/v8/finance/chart/" <> ticker
  return $ eitherDecode (response ^. responseBody)

showYahooResponse :: IO (Either String YahooResponse) -> IO ()
showYahooResponse ioEither = do
  e <- ioEither
  case e of
    Left s -> putStrLn s
    Right res -> do
      let chartResult = chart res
      print . head . result $ chartResult

showTickerInfo :: String -> IO ()
showTickerInfo = showYahooResponse . fetchTicker

getCloseOverTime :: IO (Either String YahooResponse) -> IO (Maybe [Tick])
getCloseOverTime ioEither = do
  e <- ioEither
  case e of
    Left s -> do
      putStrLn s
      return Nothing
    Right res -> do
      let chartResult = chart res
      let timestamps = timestamp . head . result $ chartResult
      let closePrices = close . head . quote . indicators . head . result $ chartResult
      return . return $ zip timestamps closePrices

showTick :: Tick -> Maybe Tick -> IO ()
showTick (_, Nothing) _ = return ()
showTick (_, Just p) (Just (_, Just prev)) =
  printf "%.2f %.2f %s\n" p diff movement
  where
    diff = p - prev
    movement :: String
    movement = if diff > 0 then " ⇑" else (if diff < 0 then " ⇓" else "")
showTick (_, Just p) _ = printf "%.2f\n" p

showTicks :: [Tick] -> IO ()
showTicks [] = return ()
showTicks (t : ts) = do
  showTick t Nothing
  showTicks' ts t
  where
    showTicks' [] _ = return ()
    showTicks' (t' : ts') prev = do
      showTick t' (Just prev)
      showTicks' ts' t'
