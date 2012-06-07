module Liberty.Common.RandomString (
  getRandomAlphanumericText
) where
import Control.Monad
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import System.Random

getRandomAlphanumericText :: Int -> IO Text
getRandomAlphanumericText len =
  let
    getRandomChar :: IO Char
    getRandomChar = do
      typeOfChar <- randomRIO (1 :: Int,3)
      randomRIO $ case typeOfChar of
        1 -> ('A', 'Z')
        2 -> ('a', 'z')
        _ -> ('0', '9')
  in do
    listOfChars <- replicateM len $ getRandomChar
    return $ LT.pack listOfChars

