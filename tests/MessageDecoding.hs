module Main where

import Network.TFTP.Message
import Network.TFTP.Types

import Control.Applicative
import Data.Word

main = do
    checkMessage (RRQ "xxx" NetASCII)
    checkMessage (WRQ "xxx" Octet)

    checkMessage (ACK 123)

    checkMessage (DATA 123 (bpack [0 .. 511]))
    checkMessage (DATA 123 (bpack []))
    checkMessage (DATA 123 (bpack [0 .. 127]))

    checkMessage (Error (ErrorMessage "test error"))
    checkMessage (Error FileNotFound)
    checkMessage (Error AccessViolation)
    checkMessage (Error DiskFull)
    checkMessage (Error IllegalTFTPOperation)
    checkMessage (Error UnknownTransferID)
    checkMessage (Error NoSuchUser)

    egasseMkcehc $ [0,1] ++ toWords0 "test.txt" ++ toWords0 "netascii"
    egasseMkcehc $ [0,5] ++ [0,0] ++ toWords0 "test error message"
    egasseMkcehc $ [0,5] ++ [0,1] ++ toWords0 ""

    testConvertMode NetASCII NetASCII
    testConvertMode NetASCII Octet
    testConvertMode Octet NetASCII
    testConvertMode Octet Octet

checkMessage m1 = do
    putStrLn $ "Message: " ++ show m1
    putStrLn $ "Encoded Message: " ++ show (encode m1)
    case decode (encode m1) of
      m | m == m1 -> putStrLn "OK"
      m -> error $ "decode . encode =/= id: " ++ (show m1) ++ " =/= " ++ (show m)

egasseMkcehc m = do
  let m1 = bpack m
      dec :: Message
      dec = decode m1
  putStrLn $ "Message: " ++ show m1
  putStrLn $ "Decoded Message: " ++ show dec
  case encode dec of
    m | m == m1 -> putStrLn "OK"
    m -> error $ "encode . decode =/= id: " ++ (show m1) ++ " =/= " ++ (show m)

testConvertMode from to = do
  let testData = bpack [0..255]
      id' = (convertMode from to) . (convertMode to from)
  case id' testData of
    t | t == testData -> putStrLn "OK"
    m2 -> error $ "convertMode not bijective: " ++ (show testData) ++ " =/= " ++ (show m2)


toWords0 :: String -> [Word8]
toWords0 str = (++ [0]) $ map (fromIntegral . fromEnum) str