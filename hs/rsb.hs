import Data.Time
import Data.Time.Clock.POSIX
import Network.C10kServer
import Network.TCPInfo
import Network.Web.Server
import Network.Web.Server.Basic
import Network.Web.URI
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
import System.FilePath
import System.IO

-- WebServer

webClosedHook :: String -> IO ()
webClosedHook s = do
    putStrLn ("closed " ++ s)

webAccessHook :: String -> IO ()
webAccessHook s = do
    putStrLn ("access " ++ s)

webErrorHook :: String -> IO ()
webErrorHook s = do
    putStrLn ("error " ++ s)

webFatalErrorHook :: String -> IO ()
webFatalErrorHook s = do
    putStrLn ("fatal error " ++ s)

webConfig = WebConfig {
      closedHook = webClosedHook
    , accessHook = webAccessHook
    , errorHook = webErrorHook
    , fatalErrorHook = webFatalErrorHook
    , connectionTimer = 1
}

-- C10K

rsbC10kInitHook :: IO ()
rsbC10kInitHook = Prelude.putStrLn "init"

rsbC10kExitHook :: String -> IO ()
rsbC10kExitHook s = Prelude.putStrLn ("exit " ++ s)

rsbC10kParentStartedHook :: IO ()
rsbC10kParentStartedHook = Prelude.putStrLn "parent started"

rsbC10kStartedHook :: IO ()
rsbC10kStartedHook = Prelude.putStrLn "started"

rsbC10kSleepTimer = 1
rsbC10kPreforkProcessNumber = 1
rsbC10kThreadNumberPerProcess = 10
rsbC10kPortName = "8200"
rsbC10kIpAddr = Nothing
rsbC10kPidFile = "/tmp/rsb.pid"
rsbC10kUser = ""
rsbC10kGroup = ""

c10kConfig = C10kConfig {
      initHook = rsbC10kInitHook
    , exitHook = rsbC10kExitHook
    , parentStartedHook = rsbC10kParentStartedHook
    , startedHook = rsbC10kStartedHook
    , sleepTimer = rsbC10kSleepTimer
    , preforkProcessNumber = rsbC10kPreforkProcessNumber
    , threadNumberPerProcess = rsbC10kThreadNumberPerProcess
    , portName = rsbC10kPortName
    , ipAddr = rsbC10kIpAddr
    , pidFile = rsbC10kPidFile
    , user = rsbC10kUser
    , group = rsbC10kGroup
}

fileGet :: FilePath -> Maybe (Integer, Integer) -> IO L.ByteString
fileGet filePath m = do
    let s = L.pack ""
    return s

fileInfo :: FilePath -> IO (Maybe (Integer, UTCTime))
fileInfo path = do 
    putStrLn ("fileInfo: " ++ path)
    return Nothing

fileMapper :: URI -> Path
fileMapper uri = File "/tmp/content"

rsbServer :: WebConfig -> Handle -> TCPInfo -> IO ()
rsbServer webConfig handle tcpinfo = do
  let config = BasicConfig { obtain = fileGet
                           , info   = fileInfo
                           , mapper = fileMapper
                           , serverName = S.pack "RSB, yo!"
                           , tcpInfo = tcpinfo
                           }
  connection handle (basicServer config) webConfig

main :: IO ()
main = do
    let
        prog = rsbServer webConfig
    runC10kServerH prog c10kConfig
