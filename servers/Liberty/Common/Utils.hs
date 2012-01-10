{-# LANGUAGE ExistentialQuantification #-}

module Liberty.Common.Utils (
  parseIntegralCheckBounds,
  fromIntegerCheckBounds
) where
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Read as LTR

parseIntegralCheckBounds :: forall a . (Integral a, Bounded a) => Text -> Maybe a
parseIntegralCheckBounds text =
  case LTR.decimal text of
    Right (parsedI, textRemaining) ->
      -- if the parse succeeded with no remaining text, return the value
      if LT.null textRemaining then
        fromIntegerCheckBounds parsedI
      else
        Nothing
    Left _ -> Nothing

fromIntegerCheckBounds :: forall a . (Integral a, Bounded a) => Integer -> Maybe a
fromIntegerCheckBounds x | toInteger (maxBound `asTypeOf` i) < toInteger x = Nothing
                    | toInteger (minBound `asTypeOf` i) > toInteger x = Nothing
                    | otherwise = Just i
  where
    i = fromIntegral x

