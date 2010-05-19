import Debug.Trace
import Network.Rest.Cli.Rsb

myAppFunction :: String -> Response String
myAppFunction s = rsbReturn s

myApp :: Uri -> Response String
myApp uri = subRequest "http://github.com/javaBin/ems/commits/master.atom" (RsbFunction "myAppFunction" myAppFunction)

main :: IO ()
main = 
    let
        req = "http://host/path"
    in
        do
            runApp req myApp
