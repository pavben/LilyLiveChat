{-# LANGUAGE ExistentialQuantification #-}

module Liberty.Common.Utils (
  fromIntegerCheckBounds,
  eitherToMaybe,
  parseUriQueryString,
  runWithRetry,
  getLocalServiceHost
) where
import Control.Arrow (second)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Liberty.Common.UrlDecode

fromIntegerCheckBounds :: forall a . (Integral a, Bounded a) => Integer -> Maybe a
fromIntegerCheckBounds x | toInteger (maxBound `asTypeOf` i) < toInteger x = Nothing
                         | toInteger (minBound `asTypeOf` i) > toInteger x = Nothing
                         | otherwise = Just i
  where
    i = fromIntegral x

eitherToMaybe :: forall a b . Either a b -> Maybe b
eitherToMaybe (Right b) = Just b
eitherToMaybe (Left _) = Nothing

-- Format: a=1&b=2 (no leading question mark)
parseUriQueryString :: ByteString -> Maybe [(ByteString, ByteString)]
parseUriQueryString uriQueryString =
  let
    encodedListOfPairs = map (second (LBS.drop 1) . C8.span (/= '=')) $ C8.split '&' uriQueryString
    decodeValueMaybe (key, encodedValue) =
      case urlDecode encodedValue of
        Just decodedValue -> Just (key, decodedValue)
        Nothing -> Nothing
  in
    mapM decodeValueMaybe encodedListOfPairs

runWithRetry :: Integer -> IO Bool -> IO Bool
runWithRetry numAttempts functionToRun = runWithRetry' numAttempts
  where
    runWithRetry' attemptsRemaining | attemptsRemaining == 0 = return False
    runWithRetry' attemptsRemaining = do
      runResult <- functionToRun
      case runResult of
        True -> return True
        False -> runWithRetry' (attemptsRemaining - 1)

getLocalServiceHost :: String -> String
getLocalServiceHost serviceName = serviceName ++ ".local.lilylivechat.net"

