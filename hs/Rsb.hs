module Rsb(
          runApp
        , subRequest
        , rsbReturn
        , Response
        , RsbFunction(..)
        , RsbApplication
        , Uri) where

import Debug.Trace
import qualified Network.HTTP as HTTP

type Uri = String
type AtomDocument = String

type RsbApplication = Uri -> Response String

data RsbFunction a = RsbFunction {
      functionName :: String
    , f :: (a -> Response a)
}

type Headers = [(String, String)]

data Response a = Transformation {
      transformationUri :: Uri
    , transformationFunction :: RsbFunction a
--    , transformationHeaders :: Headers
} | ConstResponse {
      constValue :: a
    , constHeaders :: Headers
}

headers :: Response a -> Headers
headers (Transformation _ _) = error("Transformations does not have headers")
headers (ConstResponse _ headers) = headers

rsbReturn :: a -> Response a
rsbReturn a = ConstResponse {
      constValue = a
    , constHeaders = []
}

subRequest :: Uri -> RsbFunction a -> Response a
subRequest uri function = Transformation {
          transformationUri = uri
        , transformationFunction = function
    }

runTransformation :: Uri -> (String -> Response String) -> IO (Headers, String)
runTransformation uri function = do
    putStrLn ("OUT: " ++ uri)
    result <- HTTP.simpleHTTP (HTTP.getRequest uri)
    case result of
        Left _ -> 
            error("Connection error")
        Right rsp -> 
            let 
            in do
                httpBody <- HTTP.getResponseBody result
                putStrLn ("OUT: " ++ show x ++ show y ++ show z ++ " " ++ (show reason))
                transformedResponse <- return (function httpBody)
                headers <- return (headers transformedResponse)
                return (headers, (constValue transformedResponse))
            where
                reason = HTTP.rspReason rsp
                (x,y,z) = HTTP.rspCode rsp

resolveResult :: Response String -> IO (Headers, String)
resolveResult (Transformation uri (RsbFunction name f)) = do
        putStrLn ("Transforming " ++ uri ++ " with " ++ name)
        runTransformation uri f

runApp :: Uri -> RsbApplication -> IO ()
runApp inUri app = 
    let
        applicationResult = app inUri
    in do
        putStrLn "Running..."
        putStrLn ("IN URI: " ++ inUri)
        (headers, body) <- resolveResult applicationResult
        putStrLn ("Headers: " ++ (show headers))
        putStrLn ("-----------------------")
        putStrLn body
