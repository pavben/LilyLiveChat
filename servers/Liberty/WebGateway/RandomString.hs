module Liberty.WebGateway.RandomString (
  getRandomText128
) where
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Data.Word
import Numeric
import System.Random

getRandomText128 :: IO Text
getRandomText128 = 
  let
    randomWord64AsHex = do
      ri <- randomRIO (0 :: Integer, 2 ^ (64 :: Int))
      return $ LT.pack $ showHex (fromIntegral ri :: Word64) ""
  in do
    r1 <- randomWord64AsHex
    r2 <- randomWord64AsHex
    return $ LT.concat [r1, r2]

