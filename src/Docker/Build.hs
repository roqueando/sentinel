module Docker.Build () where

import Network.HTTP.Client
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.IO (stdout)
import Control.Monad (void)

tarPath :: FilePath
tarPath = "context.tar"

socketPath :: FilePath
socketPath = "/var/run/docker.sock"

imageName :: String
imageName = "sentinel_image:latest"

buildImage :: IO ()
buildImage = do
  manager <- newManager defaultManagerSettings
  req0 <- parseRequest ("http://localhost/build?t=" ++ imageName)
  body <- LBS.readFile tarPath

  let req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS body
        , requestHeaders = [(hContentType, "application/x-tar")]
        , responseTimeout = responseTimeoutNone
        }
