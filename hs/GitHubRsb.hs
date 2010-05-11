import Debug.Trace
import Network.HTTP
import qualified Network.HTTP as HTTP
import Network.HTTP.Base
import Network.Web.URI
import Network.Rest.Rsb
import qualified Network.Rest.Rsb as Rsb
import Network.Rest.C10k
import qualified Data.ByteString.Lazy.Char8 as L
import Text.Regex.Posix

findLength :: HTTP.Response a -> Maybe Integer
findLength rsp =
    case findHeader HdrContentLength rsp of
        Just str -> Just (read str)
        Nothing -> Nothing

fetch :: String -> IO String
fetch url = do
    putStrLn ("OUT: " ++ url)
    result <- simpleHTTP (getRequest url)
    case result of
        Left _ ->
            error "Connection error"
        Right rsp ->
            do
                putStrLn ("OUT: " ++ show x ++ show y ++ show z ++ " " ++ (rspReason rsp))
                case findLength rsp of
                    Just n -> putStrLn ("length: " ++ show n)
                    Nothing -> putStrLn ("length: unknown")
                getResponseBody result
            where
                (x,y,z) = rspCode rsp

directProject :: String -> Rsb.Response
directProject project =
    Rsb.Response {
                  status = (2,0,0)
                , body = b
            }
    where
        b = do
            s <- fetch ("http://github.com/javaBin/" ++ project ++ "/commits/master.atom")
            return (L.pack s) 

gitHubMatch :: Rsb.Request -> Rsb.Response
gitHubMatch request =
    case (trace ("gitHubMatch: filePath=" ++ (show path)) path) =~ "/direct-projects/([a-z]*)" of
        (_ :: String, _ :: String, _ :: String, [project] :: [String]) -> directProject project
--            trace ("gitHubMatch: project=" ++ project)
        _ -> notFound
    where
        path = show (uriPath (uri request))

gitHubApplication :: Application
gitHubApplication = Application {
    action = gitHubMatch,
    name = "GitHub"
}

main :: IO ()
main = createRsbMain gitHubApplication
