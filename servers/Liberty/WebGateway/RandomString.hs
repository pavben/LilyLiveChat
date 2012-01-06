module Liberty.WebGateway.RandomString (
  getRandomByteString128
) where
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Word
import Numeric
import System.Random

getRandomByteString128 :: IO ByteString
getRandomByteString128 = 
  let
    randomWord64AsHex = do
      ri <- randomRIO (0, 2 ^ 64) :: IO Integer
      return $ C8.pack $ showHex (fromIntegral ri :: Word64) ""
  in do
    r1 <- randomWord64AsHex
    r2 <- randomWord64AsHex
    return $ LBS.concat [r1, r2]

