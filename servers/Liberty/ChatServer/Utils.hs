module Liberty.ChatServer.Utils (
  checkTextLengthLimits,
  getRandomPersonColorHex
) where
import Control.Monad
import Data.Int
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Numeric
import System.Random

checkTextLengthLimits :: [(Text, Int64)] -> Bool
checkTextLengthLimits = all (\(str, lenLimit) -> LT.length str <= lenLimit)

getRandomPersonColorHex :: IO Text
getRandomPersonColorHex =
  let
    randomComponentHex initialString = do
      ri <- randomRIO (50 :: Int, 100) -- Note: Keep the lower bound 16 or higher! Need 2 hex digits per component.
      return $ showHex ri initialString
  in do
    hexCodeAsText <- liftM LT.pack $ randomComponentHex "" >>= randomComponentHex >>= randomComponentHex
    return $ LT.concat [LT.pack "#", hexCodeAsText]

