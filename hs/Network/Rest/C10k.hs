-- TODO: Figure out how to export the entire Application data constructor
-- module Network.Rest.Rsb (createRsbMain) where
module Network.Rest.C10k where

import Data.DateTime
import Data.Time
--import Data.Time.Clock.POSIX
--import Debug.Trace
import Network.C10kServer
import Network.Rest.Rsb
import Network.Rest.WebServer
import Network.TCPInfo
import Network.Web.Server
import Network.Web.Server.Basic
import Network.Web.URI
import qualified Data.ByteString.Char8      as S
import qualified Data.ByteString.Lazy.Char8 as L
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

webConfig :: WebConfig
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

rsbC10kSleepTimer :: Int
rsbC10kSleepTimer = 1

rsbC10kPreforkProcessNumber :: Int
rsbC10kPreforkProcessNumber = 1

rsbC10kThreadNumberPerProcess :: Int
rsbC10kThreadNumberPerProcess = 10

rsbC10kPortName :: String
rsbC10kPortName = "8200"

rsbC10kIpAddr :: Maybe String
rsbC10kIpAddr = Nothing

rsbC10kPidFile :: String
rsbC10kPidFile = "/tmp/rsb.pid"

rsbC10kUser :: String
rsbC10kUser = ""

rsbC10kGroup :: String
rsbC10kGroup = ""

c10kConfig :: C10kConfig
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

--fileGet :: FilePath -> Maybe (Integer, Integer) -> IO L.ByteString
--fileGet "/direct-projects/ems" m = do
--    return 
--fileGet filePath m = do
--    putStrLn ("filePath: " ++ filePath)
--    let s = L.pack ""
--    return s

--findResourceF :: FilePath -> Maybe (Integer, UTCTime)
--findResourceF "/direct-projects/ems" = Just (1, fromSeconds 1)
--findResourceF f = 
--    trace ("f=" ++ f)
--    Nothing

fileInfo :: (FilePath -> Maybe (IO L.ByteString)) -> FilePath -> IO (Maybe (Maybe Integer, UTCTime))
fileInfo f path = do 
    putStrLn ("fileInfo: " ++ path)
    let
        x = case (f path) of
            Just io -> Just (Nothing, fromSeconds 1)
            Nothing -> Nothing
    return x

fileMapper :: URI -> Path
fileMapper uri = File (S.unpack (uriPath uri))

--createObtain :: Application -> FilePath -> Maybe (Integer, Integer) -> IO L.ByteString
--createObtain app filePath _ = case (action app) filePath of
--    Just io -> io
--    Nothing -> error("woot?")

--basicRsbServer :: Application -> WebConfig -> Handle -> TCPInfo -> IO ()
--basicRsbServer app webCfg handle tcpinfo = do
--  let config = BasicConfig { obtain = createObtain app
--                           , info   = fileInfo (action app)
--                           , mapper = fileMapper
--                           , serverName = S.pack ("RSB: " ++ (name app))
--                           , tcpInfo = tcpinfo
--                           }
--  connection handle (basicServer config) webCfg

simpleRsbServer :: Application -> WebConfig -> Handle -> TCPInfo -> IO ()
simpleRsbServer app webCfg handle tcpinfo = do
    connection handle (simpleServer app) webCfg

createRsbMain :: Application -> IO ()
createRsbMain app = do
    let
        prog = simpleRsbServer app webConfig
    runC10kServerH prog c10kConfig
