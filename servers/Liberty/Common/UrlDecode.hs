module Liberty.Common.UrlDecode (
  urlDecode
) where 
import Control.Applicative
import Data.Bits
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word

hexEncMap :: [(Word8, Word8)]
hexEncMap = zip [0..] (map c2w "0123456789ABCDEF")

-- PERF: Consider replacing Map with something constant-time
hexDecodeMap :: Map Word8 Word8
hexDecodeMap = Map.fromList [(b, a) | (a, b) <- hexEncMap]

urlDecode :: ByteString -> Maybe ByteString
urlDecode s = fmap LBS.pack $ urlDecodeWord8Maybe (LBS.unpack s) []

urlDecodeWord8Maybe :: [Word8] -> [Word8] -> Maybe [Word8]
urlDecodeWord8Maybe s a =
  case s of
    (c:cs) | c == c2w '%' -> do
      -- we hit a %XX, so try decoding it
      let (hexCode, remainder) = splitAt 2 cs
      case hexToMaybeWord8 hexCode of
        Just word8 -> urlDecodeWord8Maybe remainder (word8 : a)
        Nothing -> Nothing -- if we can't decode the %XX, we can't decode s
    (c:cs) ->
      -- if this isn't a %XX, add it to the output and continue
      urlDecodeWord8Maybe cs (c : a)
    [] ->
      -- if we've exhausted the entire string, we're done
      Just (reverse a)

hexToMaybeWord8 :: [Word8] -> Maybe Word8
hexToMaybeWord8 [x, y] =
  case (,) <$> Map.lookup x hexDecodeMap <*> Map.lookup y hexDecodeMap of
    Just (xValue, yValue) -> Just $ (xValue `shiftL` 4) .|. yValue
    Nothing -> Nothing
hexToMaybeWord8 _ = Nothing

