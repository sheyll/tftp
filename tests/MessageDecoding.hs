module Main where

import Network.TFTP.Message
import Network.TFTP.Types

import Test.QuickCheck
import Debug.Trace

import System.Exit

main = do
    -- quick check
    qc "prop_identity_pickling" prop_identity_pickling

    -- handwritten special test cases
    checkMessage (RRQ "xxx" NetASCII)
    checkMessage (WRQ "xxx" Octet)

    checkMessage (ACK 123)

    checkMessage (DATA 123 (pack (replicate 512 42)))
    checkMessage (DATA 123 (pack []))
    checkMessage (DATA 123 (pack [0 .. 127]))

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


--------------------------------------------------------------------------------
-- QC runner
qc name action = do
  putStrLn $ "\n\nRunning QC Test: " ++ name
  let action' msg = trace (printf " %s - instance:\n%s\n\n " name (show msg))
                          (action msg)
  res <- quickCheckWithResult (stdArgs { chatty = True
                                       , maxSize = 1024
                                       , maxSuccess = 1024})
                              action'
  if isSuccess res then do
      putStrLn $ "*** QC TEST SUCCESSFUL ***\n"
      return ()
   else do
      putStrLn  "*** QC TEST ERROR ***\n\n"
      exitWith (ExitFailure 1)

isSuccess Success{} = True
isSuccess _ = False

--------------------------------------------------------------------------------
-- QC props
prop_identity_pickling :: Message -> Bool
prop_identity_pickling msg = (decode . encode) msg == msg

--------------------------------------------------------------------------------
-- QC generate random messages
instance Arbitrary Message where
    arbitrary = do
      n <- choose (1, 5) :: Gen Int
      case n of
        1 -> RRQ <$> arbitraryAscii <*> arbitrary
        2 -> WRQ <$> arbitraryAscii <*> arbitrary
        3 -> DATA <$> arbitrary <*> arbitraryChunk
        4 -> ACK <$> arbitrary
        5 -> Error <$> arbitrary

newtype AsciiStr = AsciiStr String

instance Arbitrary AsciiStr where
    arbitrary = do
      let ascii = elements (toEnum <$> [32 .. 126])
      len <- choose (1, 80)
      str <- sequence $ replicate len ascii
      return (AsciiStr str)

arbitraryAscii = do
  (AsciiStr str) <- arbitrary
  return str

newtype DataBlock = DataBlock ByteString

instance Arbitrary DataBlock where
    arbitrary = do
      (x, maxSize) <- elements [('S', 0), ('M', 511), ('L', 512)]
      chunkSize <- if maxSize == 511 then choose (1,511) else return maxSize
      return $ DataBlock $ pack $ replicate chunkSize (fromIntegral $ fromEnum x)

arbitraryChunk = do
  (DataBlock chunk) <- arbitrary
  return chunk

instance Arbitrary Mode where
    arbitrary = elements [NetASCII, Octet]

instance Arbitrary TFTPError where
    arbitrary = oneof
                [ ErrorMessage <$> arbitraryAscii
                , return FileNotFound
                , return AccessViolation
                , return DiskFull
                , return IllegalTFTPOperation
                , return UnknownTransferID
                , return FileAlreadyExists
                , return NoSuchUser]

--------------------------------------------------------------------------------

checkMessage m1 = do
    putStrLn $ "Message: " ++ show m1
    putStrLn $ "Encoded Message: " ++ show (encode m1)
    case decode (encode m1) of
      m | m == m1 -> putStrLn "OK"
      m -> error $ "decode . encode =/= id: " ++ (show m1) ++ " =/= " ++ (show m)

egasseMkcehc m = do
  let m1 = pack m
      dec :: Message
      dec = decode m1
  putStrLn $ "Message: " ++ show m1
  putStrLn $ "Decoded Message: " ++ show dec
  case encode dec of
    m | m == m1 -> putStrLn "OK"
    m -> error $ "encode . decode =/= id: " ++ (show m1) ++ " =/= " ++ (show m)

testConvertMode from to = do
  let testData = pack [0..255]
      id' = (convertMode from to) . (convertMode to from)
  case id' testData of
    t | t == testData -> putStrLn "OK"
    m2 -> error $ "convertMode not bijective: " ++ (show testData) ++ " =/= " ++ (show m2)


toWords0 :: String -> [Word8]
toWords0 str = (++ [0]) $ map (fromIntegral . fromEnum) str