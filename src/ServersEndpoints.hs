{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module ServersEndpoints (callServer, getFromServer, putFromServer, delFromServer) where

import qualified Servant.API.Verbs as Verbs
import System.Random ( getStdGen, Random(randomRs) )
import Data.Aeson (Value)
import Servant
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client

type MyPut = Verbs.Verb PUT 201
       
type API = 
      "storage" :> Capture "my-file" FilePath :> Get '[JSON] Value
       :<|> "storage" :> Capture "my-file" FilePath :> ReqBody '[JSON] Value :> MyPut '[JSON] ()
       :<|> "storage" :> Capture "my-file" FilePath :> DeleteNoContent '[JSON] ()

resApi :: Proxy API
resApi = Proxy

getFromServer :: FilePath -> ClientM Value
putFromServer :: FilePath -> Value -> ClientM ()
delFromServer :: FilePath -> ClientM ()
getFromServer :<|> putFromServer :<|> delFromServer = client resApi

callServer :: Show a => ClientM a -> IO (Either ClientError a) 
callServer task = do
    g <- getStdGen
    let roll = head (randomRs (1 :: Int, 3 :: Int) g)
    case roll of
      1 -> call 8081
      _ -> call 8080 >>= pure
    where
      call port = do
        manager' <- newManager defaultManagerSettings
        runClientM task (mkClientEnv manager' (BaseUrl Http "localhost" port ""))
