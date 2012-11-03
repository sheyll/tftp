module Main where

import Network.TFTP.Message

main = do
    checkMessage (RRQ "xxx" NetASCII)
    checkMessage (WRQ "xxx" Octet)
    checkMessage (ACK 123)
    checkMessage (DATA 123 (pack [0 .. 64]))
    checkMessage (Error 42 "B0rked!1")

checkMessage m1 = do
    putStrLn $ "Message: " ++ show m1
    putStrLn $ "Encoded Message: " ++ show (encode m1)
    case decode (encode m1) == m1 of
      True -> putStrLn "OK"