{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Liberty.Common.GeoLocation(
  getLocationForIp
) where
import Control.Applicative
import qualified Data.Aeson as J
import Data.Attoparsec.Number as DAN
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Network.HTTP
import Network.URI

getLocationForIp :: Text -> IO (Maybe (Text, Text, Text))
getLocationForIp ip =
  case parseURI ("http://www.geoplugin.net/json.gp?ip=" ++ LT.unpack ip) of
    Just uri ->
      let
        request = Request {
          rqURI = uri,
          rqMethod = GET,
          rqHeaders = [],
          rqBody = LBS.empty
        }
      in do
        getLocationForIpResponse <- simpleHTTP request
        case getLocationForIpResponse of
          Right response ->
            let
              responseWithoutFirst = LBS.drop 10 $ rspBody response
              responseJS = LBS.take (LBS.length responseWithoutFirst - 1) $ responseWithoutFirst
            in do
              case J.decode responseJS of
                Just (responseObject :: J.Object) ->
                  case (,,,) <$>
                    HMS.lookup "geoplugin_status" responseObject <*>
                    HMS.lookup "geoplugin_city" responseObject <*>
                    HMS.lookup "geoplugin_regionName" responseObject <*>
                    HMS.lookup "geoplugin_countryName" responseObject
                  of
                    Just (
                      J.Number (DAN.I 200),
                      J.String (LT.fromStrict -> city),
                      J.String (LT.fromStrict -> regionName),
                      J.String (LT.fromStrict -> countryName)
                      ) ->
                      return $ Just (city, regionName, countryName)
                    _ -> do
                      putStrLn "Cannot obtain location for IP"
                      return Nothing
                _ -> do
                  putStrLn "Cannot decode response object"
                  return Nothing
          Left connError -> do
            putStrLn $ "Error sending getLocationForIp API request: " ++ show connError
            return Nothing
    Nothing -> do
      putStrLn "ASSERT: Cannot parse our hardcoded URI in getLocationForIp"
      return Nothing

