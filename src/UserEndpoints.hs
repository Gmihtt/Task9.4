{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module UserEndpoints (app) where

import Control.Monad.Except (liftIO)
import Data.Maybe ( fromMaybe )
import Data.Aeson (Value)
import Network.HTTP.Types.Status (Status (statusCode))
import Network.Wai
import Servant
import qualified Servant.API.Verbs as Verbs
import Servant.Client
import ServersEndpoints (callServer, delFromServer, getFromServer, putFromServer)

type MyPut = Verbs.Verb PUT 201

type API =
  "storage" :> Capture "my-file" FilePath
    :> ( Get '[JSON] Value
           :<|> ReqBody '[JSON] Value :> MyPut '[JSON] ()
           :<|> DeleteNoContent '[JSON] ()
       )

api :: Proxy API
api = Proxy

getCode :: ResponseF a -> Maybe ServerError
getCode resp =
  case (statusCode . responseStatusCode) resp of
    204 -> Nothing
    400 -> Just err400
    415 -> Just err415
    _ -> Just err404

parseClientError :: ClientError -> Maybe ServerError
parseClientError (FailureResponse _ resp) = getCode resp
parseClientError (DecodeFailure _ resp) = getCode resp
parseClientError (UnsupportedContentType _ resp) = getCode resp
parseClientError (InvalidContentTypeHeader resp) = getCode resp
parseClientError _ = Just err404

server :: Server API
server file = get file :<|> put file :<|> del file
  where
    get :: FilePath -> Handler Value
    get file = do
      res <- liftIO $ callServer (getFromServer file)
      either (throwError . fromMaybe err404 . parseClientError) pure res

    put :: FilePath -> Value -> Handler ()
    put file req = do
      res <- liftIO $ callServer (putFromServer file req)
      either (maybe (pure ()) throwError . parseClientError) pure res

    del :: FilePath -> Handler ()
    del path = do
      res <- liftIO $ callServer (delFromServer path)
      liftIO $ print res
      either (maybe (pure ()) throwError . parseClientError) pure res

app :: Application
app = serve api server
