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
    [LT.pack -> siteId, LT.pack -> adminPassword] -> setSiteAdminPassword siteId adminPassword
    _ -> do
      progName <- getProgName
      putStrLn $ "Usage: " ++ progName ++ " siteId newAdminPassword"

  return ()

setSiteAdminPassword :: Text -> Text -> IO ()
setSiteAdminPassword siteId adminPassword = do
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
            sendMessageToService CSMTSASetSiteAdminPassword (
              siteId,
              adminPassword) serviceHandle

            createResponse <- receiveMessageFromService serviceHandle
            case createResponse of
              (CSMTSASetSiteAdminPasswordSuccess,_) -> liftIO $ putStrLn "Site admin password changed successfully."
              (CSMTSASetSiteAdminPasswordFailed,_) -> liftIO $ putStrLn "Password change request was rejected by the Chat Server"
              _ -> liftIO $ putStrLn "Unknown message received from Chat Server"
          _ -> return ()
      return ()
    SLNotAvailable -> putStrLn "Site Locator Service not available"
