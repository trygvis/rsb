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
      status :: (Int, Int, Int) -- TODO: Create a type
    , responseLength :: Maybe Int
    , body :: IO L.ByteString
}

subRequest :: URI -> Request -> (Response -> a) -> Response
subRequest uri originalRequest f = error (show uri)

notFound :: Response
notFound = 
    Response {
          status = (4,0,4)
        , body = return b
        , responseLength = Nothing
    }
    where
        b = L.pack ("not found")
