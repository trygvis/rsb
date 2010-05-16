import Debug.Trace

type RsbBody = String
type Uri = String
type AtomDocument = String

data Response = SingleResponse {
    body :: String
}

myApp = subRequest "http://github.com/javaBin/ems/commits/master.atom" (rsbReturn id)

rsbReturn :: a -> Response a
rsbReturn a = Response {
    body = a
}

subRequest :: Uri -> (String -> Response String) -> SingleResponse String
subRequest req function = SingleResponse {
          resReq = req
        , resFunction = function
    }

-- req for 'http://foo' with 'repositoryList':
--   1: 'http://foo/path1' with 'atom'
--   2: 'http://foo/path2' with 'atom'
show :: Response a b -> String
show response = "Request for '" ++ (reqUri (resReq response)) ++ "' did these sub requests: \n" 

runApp :: AppRequest -> RsbFunction String String -> IO ()
runApp appReq app = 
    let
        req = Request (appReqUri appReq)
        f = (rsbFunction app)
    in do
        putStrLn "Running..."
        putStrLn ("IN URI: " ++ (appReqUri appReq))
        body <- return "the body"

--        putStrLn (resBody result1)
        putStrLn "Done"

main :: IO ()
main = 
    let
        req = AppRequest { appReqUri = "http://host/path" }
    in
        do
            runApp req app1
