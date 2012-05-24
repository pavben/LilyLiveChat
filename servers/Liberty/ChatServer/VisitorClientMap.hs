module Liberty.ChatServer.VisitorClientMap (
  VisitorClientMapTVar,
  initializeVisitorClientMap,
  addCustomerToVisitorClientMap,
  getClientDataTVarsForVisitorId,
  removeCustomerFromVisitorClientMap
) where
import Control.Concurrent.STM.TVar
import Control.Monad.STM
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text.Lazy (Text)
import Liberty.ChatServer.Types
import Debug.Trace

-- public data
type VisitorClientMapTVar = TVar (Map Text [ClientDataTVar])

initializeVisitorClientMap :: IO (VisitorClientMapTVar)
initializeVisitorClientMap =
  atomically $ newTVar Map.empty

addCustomerToVisitorClientMap :: ClientDataTVar -> VisitorClientMapTVar -> STM ()
addCustomerToVisitorClientMap clientDataTVar visitorClientMapTVar = do
  clientData <- readTVar clientDataTVar
  case cdOtherData clientData of
    OCDClientCustomerData clientCustomerData ->
      case ccdVisitorId clientCustomerData of
        Just visitorId -> do
          visitorClientMap <- readTVar visitorClientMapTVar
          case Map.lookup visitorId visitorClientMap of
            Just clientDataTVars ->
              -- if visitorId already exists, add this clientDataTVar to the list
              writeTVar visitorClientMapTVar $ Map.insert visitorId (clientDataTVar : clientDataTVars) visitorClientMap
            Nothing ->
              -- if visitorId does not exist, create it
              writeTVar visitorClientMapTVar $ Map.insert visitorId [clientDataTVar] visitorClientMap
        -- if the client does not have a visitorId, there's nothing to do
        Nothing -> return ()
    -- if the given client is not a customer, we have a bug
    _ -> return $ trace "ASSERT: addCustomerToVisitorClientMap given a client that is not a customer" ()

getClientDataTVarsForVisitorId :: Text -> VisitorClientMapTVar -> STM [ClientDataTVar]
getClientDataTVarsForVisitorId visitorId visitorClientMapTVar = do
  visitorClientMap <- readTVar visitorClientMapTVar
  case Map.lookup visitorId visitorClientMap of
    Just clientDataTVars -> return clientDataTVars
    Nothing -> return []

removeCustomerFromVisitorClientMap :: ClientDataTVar -> VisitorClientMapTVar -> STM ()
removeCustomerFromVisitorClientMap clientDataTVar visitorClientMapTVar = do
  clientData <- readTVar clientDataTVar
  case cdOtherData clientData of
    OCDClientCustomerData clientCustomerData ->
      case ccdVisitorId clientCustomerData of
        Just visitorId -> do
          visitorClientMap <- readTVar visitorClientMapTVar
          case Map.lookup visitorId visitorClientMap of
            Just clientDataTVars ->
              let
                clientDataTVarsAfterFilter = filter (/= clientDataTVar) clientDataTVars
              in
                if null $ clientDataTVarsAfterFilter then
                  -- we've just removed the last value from this key, so remove the key
                  writeTVar visitorClientMapTVar $ Map.delete visitorId visitorClientMap
                else
                  -- there are still some values remaining in the key, so write them
                  writeTVar visitorClientMapTVar $ Map.insert visitorId clientDataTVarsAfterFilter visitorClientMap
            Nothing -> return $ trace "ASSERT: removeCustomerFromVisitorClientMap cannot find customer's visitorId in visitorClientMap" ()
        -- if the client does not have a visitorId, there's nothing to do
        Nothing -> return ()
    -- if the given client is not a customer, we have a bug
    _ -> return $ trace "ASSERT: removeCustomerToVisitorClientMap given a client that is not a customer" ()

