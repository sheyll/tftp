-- | High-level API for building simple TFTP Servers, currently restricted to answering read requests.
module Network.TFTP.Server(singleBinary) where

import Prelude hiding (readFile, catch)

import Control.Exception(Exception, catch, IOException)
import Network.TFTP.Protocol
import Network.TFTP.Types
import Network.TFTP.UDPIO
import System.Directory(doesFileExist)

-- | Create a simple server that answers a single read request from a single client for
-- a file, and return 'Nothing' when the transfer was successfully completed,
-- or 'Just <error message>'
singleBinary :: Maybe Int
                -> FilePath
                -> String
                -> Maybe String
                -> Maybe String
                -> IO (Maybe String)
singleBinary timeout fname alias host port = do
  fileExists <- doesFileExist fname
  if not fileExists then
    return (Just "File not found.")
   else
    catch runServer handleExc
  where
    runServer = do
      cont <- readFile fname
      infoM "TFTPUpload"
        (printf "Serving file '%s' under the name '%s' on host '%s' port '%s'"
         fname alias (show host) (show port))
      res <- udpIO host port
             (runTFTP (offerSingleFile timeout alias cont))
      if res then
        return Nothing
        else
        return (Just "File Upload failed.")

    handleExc :: IOException -> IO (Maybe String)
    handleExc e = do
      let err = show e
      errorM "TFTP.Server" err
      return (Just err)
