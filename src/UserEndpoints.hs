{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module UserEndpoints (app) where

import Control.Monad.Except (liftIO)
import qualified Servant.API.Verbs as Verbs
import Data.Aeson (Value)
import Network.Wai
import Servant
import Servant.Client
import Network.HTTP.Types.Status ( Status(statusCode) )
import ServersEndpoints (callServer, getFromServer, putFromServer, delFromServer)

type MyPut = Verbs.Verb PUT 201

type API = 
      "storage" :> Capture "my-file" FilePath :> (Get '[JSON] Value
       :<|> ReqBody '[JSON] Value :> MyPut '[JSON] ()
       :<|> DeleteNoContent '[JSON] ())

api :: Proxy API
api = Proxy

getCode :: ResponseF a -> ServerError
getCode resp = 
    case (statusCode . responseStatusCode) resp of
      400 -> err400
      415 -> err415
      _ -> err404

parseClientError :: ClientError -> ServerError
parseClientError (FailureResponse _ resp) = getCode resp
parseClientError (DecodeFailure _ resp) = getCode resp
parseClientError (UnsupportedContentType _ resp) = getCode resp
parseClientError (InvalidContentTypeHeader resp) = getCode resp
parseClientError (ConnectionError _) = err404

server :: Server API 
server file = get file :<|> put file :<|> del file
  where
    get :: FilePath -> Handler Value
    get file = do
      res <- liftIO $ callServer (getFromServer file)
      either (throwError . parseClientError) pure res

    put :: FilePath -> Value -> Handler ()
    put file req = do
      res <- liftIO $ callServer (putFromServer file req)
      either (throwError . parseClientError) pure res

    del :: FilePath -> Handler ()
    del path = do
      res <- liftIO $ callServer (delFromServer path)
      either (throwError . parseClientError) pure res

app :: Application
app req respond = do
  liftIO $ print req
  serve api server req respond
