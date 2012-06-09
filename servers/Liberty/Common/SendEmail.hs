{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Liberty.Common.SendEmail(
  sendEmail
) where
import qualified Data.Aeson as J
import Data.Attoparsec.Number as DAN
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMS
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
import Network.HTTP
import Network.URI

sendEmail :: Text -> Text -> Text -> IO ()
sendEmail to subject textBody =
  case parseURI "http://api.postmarkapp.com/email" of
    Just uri ->
      let
        encodedRequestObject = J.encode $ J.object [
          ("From", "LilyLiveChat <mail@lilylivechat.net>"),
          ("To", J.String $ LT.toStrict to),
          ("Subject", J.String $ LT.toStrict subject),
          ("TextBody", J.String $ LT.toStrict textBody)
          ]
        request = Request {
          rqURI = uri,
          rqMethod = POST,
          rqHeaders = [
            mkHeader HdrContentType "application/json",
            mkHeader HdrContentLength $ show $ LBS.length encodedRequestObject,
            mkHeader (HdrCustom "X-Postmark-Server-Token") "d57dad37-d6bd-4b9a-addd-bcb77b5e9ad5"
            ],
          rqBody = encodedRequestObject
        }
      in do
        sendEmailResponse <- simpleHTTP request
        case sendEmailResponse of
          Right response -> do
            case J.decode $ rspBody response of
              Just (responseObject :: J.Object) ->
                case HMS.lookup "ErrorCode" responseObject of
                  Just (J.Number (DAN.I 0)) ->
                    putStrLn "E-mail sent successfully"
                  _ -> do
                    putStrLn "E-mail send failed. Response is:"
                    print responseObject
              Nothing ->
                putStrLn $ "Error decoding e-mail API response"
          Left connError ->
            putStrLn $ "Error sending e-mail API request: " ++ show connError
    Nothing ->
      putStrLn "ASSERT: Cannot parse our hardcoded URI in sendEmail"

