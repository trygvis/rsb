module Network.Rest.WebServer(simpleServer) where

import Data.ByteString.Char8(pack)
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Applicative
import Control.Monad
import Network.Web.HTTP hiding(Request, Response)
import qualified Network.Web.HTTP as HTTP(Request, Response)
import Network.Web.Server
import Network.Web.Server.Params
import Network.Rest.Rsb
import qualified Network.Rest.Rsb as Rsb
import Data.Maybe

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
    let
        appRequest = Rsb.Request {
            uri = reqURI req
        }
        appResponse = (action app) appRequest
        headers = createHeaders appResponse
--        body = fmap (\io -> ) (responseBody appResponse)
        body = return (L.pack "")
    return (makeResponse2 OK body len headers)
    where
        len = Nothing

createHeaders :: Response -> [(FieldKey, FieldValue)]
createHeaders response =
        catMaybes [server, contentLength]
    where
        server = Just (FkServer, pack "RSB")
        contentLength = fmap (\len -> (FkContentLength, pack (show len))) (responseContentLength response)
