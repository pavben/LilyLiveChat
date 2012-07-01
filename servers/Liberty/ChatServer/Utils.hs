module Liberty.ChatServer.Utils (
  checkTextLengthLimits
) where
import Data.Int
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT

checkTextLengthLimits :: [(Text, Int64)] -> Bool
checkTextLengthLimits = all (\(str, lenLimit) -> LT.length str <= lenLimit)

