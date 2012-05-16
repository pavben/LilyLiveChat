module Liberty.ChatServer.Utils (
  hashTextWithSalt,
  checkTextLengthLimits,
  getRandomPersonColorHex
) where
import Control.Monad
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.Int
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Numeric
import System.Random
import Debug.Trace

hashTextS512 :: Text -> Text
hashTextS512 text =
  case LTE.decodeUtf8' $ LBS.fromChunks [Base16.encode $ SHA512.hashlazy $ LTE.encodeUtf8 text] of
    Right textS512 -> textS512
    Left _ -> trace "ASSERT: Error in decodeUtf8' in hashTextS512" LT.empty

hashTextWithSalt :: Text -> Text
hashTextWithSalt text = hashTextS512 $ LT.append text $ LT.pack "$#=LilyLiveChat-%!"

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

