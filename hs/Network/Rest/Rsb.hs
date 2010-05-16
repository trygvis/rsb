-- TODO: Figure out how to export the entire RsbApplication data constructor
-- module Network.Rest.Rsb (createRsbMain, RsbApplication) where
module Network.Rest.Rsb where

import Network.Web.Server
import Network.Web.Server.Basic
import Network.Web.URI
import qualified Data.ByteString.Lazy.Char8 as L

data Application = Application {
      action :: (Request -> Response)
    , name :: String    
}

data Request = Request {
      uri :: URI
}

data Response = Response {
      responseStatus :: (Int, Int, Int) -- TODO: Create a type
    , responseContentLength :: Maybe Int
    , responseBody :: Maybe (IO L.ByteString)
}

subRequest :: URI -> Request -> (Response -> a) -> Response
subRequest uri originalRequest f = error (show uri)

defaultResponse = Response {
          responseStatus = (5,0,0)
        , responseBody = Just (return (L.pack "internal error"))
        , responseContentLength = Nothing
    }

notFound :: Response
notFound = 
    defaultResponse {
          responseStatus = (4,0,4)
        , responseBody = Just (return b)
    }
    where
        b = L.pack ("not found")
