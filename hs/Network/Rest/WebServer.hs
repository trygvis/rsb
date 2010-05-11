module Network.Rest.WebServer(simpleServer) where

import Data.ByteString.Char8(pack)
import Control.Applicative
import Control.Monad
import Network.Web.HTTP hiding(Request, Response)
import qualified Network.Web.HTTP as HTTP(Request, Response)
import Network.Web.Server
import Network.Web.Server.Params
import Network.Rest.Rsb

simpleServer :: Application -> WebServer
simpleServer cnf mreq = case mreq of
    Nothing -> pure responseBadRequest
    Just req -> case reqMethod req of
      GET   -> processGET  cnf req
      _     -> pure responseNotImplement

responseBadRequest :: HTTP.Response
responseBadRequest = makeResponse BadRequest []
responseNotImplement :: HTTP.Response
responseNotImplement = makeResponse NotImplemented []

processGET :: Application -> HTTP.Request -> IO HTTP.Response
processGET app req = do
    return (makeResponse2 OK body len (createHeaders response))
    where
        response = action app
        body = Nothing
        len = Nothing

createHeaders :: Response -> [(FieldKey, FieldValue)]
createHeaders response =
        filter (\m -> case m of 
                Just x -> True 
                _ -> False) maybeHeaders
    where
        maybeHeaders = [ Just (FkServer, pack "RSB"), fmap (\len -> (FkContentLength, pack (show len)) (responseLength response)) ]
