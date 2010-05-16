import Rsb
import Debug.Trace

x :: String -> Response String
x s = rsbReturn s

myApp :: Uri -> Response String
myApp uri = 
    let
        f = rsbReturn
    in
        subRequest "http://github.com/javaBin/ems/commits/master.atom" (RsbFunction "myAppFunction" x)

main :: IO ()
main = 
    let
        req = "http://host/path"
    in
        do
            runApp req myApp
