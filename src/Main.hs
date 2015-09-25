{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.TZworld.Api

import           Data.ByteString.Char8
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant

type TzAPI = "location" :> QueryParam "lat" String :> QueryParam "lon" String :> Get '[JSON]  TimeZone

main::IO ()
main=run 8004 app


server :: Server TzAPI
server = handlit
  where handlit :: Maybe String -> Maybe String -> EitherT ServantErr IO TimeZone
        handlit lap lop = case (lap,lop) of
          (Nothing,Nothing) -> sendError404 "No latitude or longitude values provided"
          (Nothing, _)      -> sendError404 "No latitude value was provided"
          (_,Nothing)       -> sendError404 "No longititude value was provided"
          (Just la, Just lo)-> do
            tze <- liftIO $ handleLocation (pack la) (pack lo)
            case tze of
              Left err -> sendError404 err
              Right tz -> return tz

sendError404 :: Monad m =>String -> EitherT ServantErr m a
sendError404 msg = left err404 {errBody = encode $ Message msg}

tzAPI :: Proxy TzAPI
tzAPI = Proxy

app :: Application
app = serve tzAPI server
