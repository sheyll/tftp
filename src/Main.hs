module Main where

import Network.TFTP.Server

main = serveFiles "127.0.0.1" 17777 "/tmp" 0
