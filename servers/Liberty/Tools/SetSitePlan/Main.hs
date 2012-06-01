{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}

module Main
where
import Control.Monad.IO.Class
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import System.Environment
import Liberty.Common.Messages.ChatServer
import Liberty.Common.Messages.SiteLocatorService
import Liberty.Common.ServiceClient

main :: IO ()
main = do
  commandLineArgs <- getArgs
  case commandLineArgs of
    [LT.pack -> siteId, read -> planId] -> setSitePlan siteId planId
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " siteId planId"

  return ()

setSitePlan :: Text -> Int -> IO ()
setSitePlan siteId planId = do
  -- locate the server that the site is on
  siteLocateResult <- locateSite siteId
  case siteLocateResult of
    SLSuccess serverId -> do
      putStrLn $ "Site located on server: " ++ LT.unpack serverId
      -- get ServiceConnectionData from the server name
      let chatServerConnectionData = getServiceConnectionDataForChatServer serverId
      _ <- withServiceConnection chatServerConnectionData $ \serviceHandle -> do
        -- login as super admin
        sendMessageToService CSSALoginRequestMessage (LT.pack "ZGZqanZvaWVpc3VnaGRzZnJhZWhmcWgzcTRxcmZhd3dmMkAhIUBAIQoK") serviceHandle

        loginResponse <- receiveMessageFromService serviceHandle
        case loginResponse of
          (CSSALoginSuccessMessage,_) -> do
            -- execute the password change command
            sendMessageToService CSMTSASetSitePlan (
              siteId,
              planId) serviceHandle

            createResponse <- receiveMessageFromService serviceHandle
            liftIO $ putStrLn $ "Response: " ++ show (fst createResponse :: ChatServerMessageType)
          _ -> return ()
      return ()
    SLNotAvailable -> putStrLn "Site Locator Service not available"
