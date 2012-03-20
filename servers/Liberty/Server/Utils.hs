module Liberty.Server.Utils (
  hashTextWithSalt
) where
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.Encoding as LTE
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Debug.Trace

hashTextS512 :: Text -> Text
hashTextS512 text =
  case LTE.decodeUtf8' $ LBS.fromChunks [Base16.encode $ SHA512.hashlazy $ LTE.encodeUtf8 text] of
    Right textS512 -> textS512
    Left _ -> trace "ASSERT: Error in decodeUtf8' in hashTextS512" LT.empty

hashTextWithSalt :: Text -> Text
hashTextWithSalt text = hashTextS512 $ LT.append text $ LT.pack "$#=LilyLiveChat-%!"

