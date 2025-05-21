module Docker.Compact (createTarball) where

import qualified Codec.Archive.Tar as Tar
import System.Directory
import System.Process
import Data.List ((\\))

createTarball :: IO ()
createTarball = do
  files <-  getNotIgnoredFiles
  currentDir <- getCurrentDirectory
  Tar.create "context.tar" currentDir files
  putStrLn "tarball created successfully"

listAllFiles :: IO [FilePath]
listAllFiles = do
  output <- readProcess "find" [".", "-type", "f"] ""
  return $ lines output

checkIgnored :: [FilePath] -> IO [FilePath]
checkIgnored paths = do
  let input = unlines paths
  ignored <- readProcess "git" ["check-ignore", "--stdin"] input
  return $ lines ignored

getNotIgnoredFiles :: IO [FilePath]
getNotIgnoredFiles = do
  allFiles <- listAllFiles
  ignored <- checkIgnored allFiles
  return (allFiles \\ ignored)


