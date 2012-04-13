{-# LANGUAGE ExistentialQuantification #-}

module Liberty.Common.Utils (
  parseIntegralCheckBounds,
  parseIntegral,
  fromIntegerCheckBounds,
  eitherToMaybe,
  parseUriQueryString
) where
import Control.Arrow (second)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Read as LTR
import Liberty.Common.UrlDecode

parseIntegralCheckBounds :: forall a . (Integral a, Bounded a) => Text -> Maybe a
parseIntegralCheckBounds text = do
  case parseIntegral text of
    Just parsedI -> fromIntegerCheckBounds parsedI
    Nothing -> Nothing

parseIntegral :: Integral a => Text -> Maybe a
parseIntegral text =
  case LTR.decimal text of
    Right (parsedI, textRemaining) ->
      -- if the parse succeeded with no remaining text, return the value
      if LT.null textRemaining then
        Just $ parsedI
      else
        Nothing
    Left _ -> Nothing

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

