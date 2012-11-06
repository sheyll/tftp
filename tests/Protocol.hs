module Main where

import Network.TFTP.Protocol
import Network.TFTP.Types
import qualified Network.TFTP.Message as M
import System.IO(stdout)

import Debug.Trace
import System.Exit

main = do
  init_logging
  l "\n\nRunning: testReceive"                  >> runXFerMock testReceive
  l "\n\nRunning: testReplyData"                >> runXFerMock testReplyData
  l "\n\nRunning: testSendData"                 >> runXFerMock testSendData
  l "\n\nRunning: testIncIndex"                 >> runXFerMock testIncIndex
  l "\n\nRunning: testContinueAfterACK_SUCCESS" >> runXFerMock testContinueAfterACK_SUCCESS
  l "\n\nRunning: testContinueAfterACK_RETRY"   >> runXFerMock testContinueAfterACK_RETRY
  l "\n\nRunning: testContinueAfterACK_ERROR1"  >> runXFerMock testContinueAfterACK_ERROR1
  l "\n\nRunning: testContinueAfterACK_ERROR2"  >> runXFerMock testContinueAfterACK_ERROR2
  l "\n\nRunning: testContinueAfterACK_ERROR2"  >> runXFerMock testContinueAfterACK_ERROR2
  l "\n\nRunning: testWriteData_oneblock"       >> runXFerMock testWriteData_oneblock
  l "\n\nRunning: testWriteData_invalid_ack"    >> runXFerMock testWriteData_invalid_ack
  l "\n\nRunning: testWriteData_manyblocks 0"   >> runXFerMock (testWriteData_manyblocks 0)
  l "\n\nRunning: testWriteData_manyblocks 1"   >> runXFerMock (testWriteData_manyblocks 9)

-- tests

testWriteData_manyblocks lastChunkSize = do
  let blob      = bpack $ concat ((replicate (blocks - 1) chunk) ++ [lastChunk])
      chunk     = replicate 512 42
      lastChunk = replicate lastChunkSize 43
      blocks    = 100
      peer      = 123
      lastIdx   = fromIntegral $ blocks - 1

  sequence $
   [do mock $ ExpectSend    peer (M.DATA index (bpack chunk))
       mock $ ExpectReceive peer (M.ACK index)
   | index <- fromIntegral <$> [0 .. (blocks - 2)]]

  -- The transfer is closed by sending an empty data message
  mock $ ExpectSend    peer (M.DATA lastIdx (bpack lastChunk))
  mock $ ExpectReceive peer (M.ACK lastIdx)

  setLastPeer $ Just peer
  writeData blob
  verify

testWriteData_invalid_ack = do
  let blob = bpack (replicate 25 (65 + 11))
      peer = 123

  sequence $ replicate (maxRetries + 1) $ do
    mock $ ExpectSend peer (M.DATA 0 blob)
    mock $ ExpectReceive peer (M.ACK 666)
  mock $ ExpectSend peer (M.Error (M.ErrorMessage "timeout"))

  setLastPeer $ Just peer
  writeData blob
  verify


testWriteData_oneblock = do
  let testChunk = bpack (replicate 255 (65 + 11))
      peer = 123

  mock $ ExpectSend peer $ M.DATA 0 testChunk
  mock $ ExpectReceive peer (M.ACK 0)

  setLastPeer $ Just peer
  writeData testChunk
  verify


testContinueAfterACK_SUCCESS = do
  idx <- incBlockIndex
  mock $ ExpectReceive 1 (M.ACK idx)
  continueAfterACK (return ())
                   (testFail "retry called")
                   (testFail "fail called")
  verify

testContinueAfterACK_RETRY = do
  idx <- incBlockIndex
  mock $ ExpectReceive 1 (M.ACK (idx + 10))
  continueAfterACK (testFail "success called")
                   (return ())
                   (testFail "fail called")
  verify

testContinueAfterACK_ERROR1 = do
  idx <- incBlockIndex
  mock $ ExpectReceive 1 (M.RRQ "test.txt" M.NetASCII)
  continueAfterACK (testFail "success called")
                   (testFail "retry called")
                   (return ())
  verify

testContinueAfterACK_ERROR2 = do
  idx <- incBlockIndex
  mock $ ExpectReceive 0 (M.Error M.FileNotFound)
  continueAfterACK (testFail "success called")
                   (testFail "retry called")
                   (return ())
  verify

testReceive = do
  let m = M.RRQ "test.txt" M.NetASCII
      peer = 777
  mock $ ExpectReceive peer m
  m' <- receive
  peer' <- getLastPeer
  assertEqual "testReceive wrong peer" (Just peer) peer'
  assertEqual "testReceive invalid mesage" m m'
  verify

testReplyData = do
    let chunk = bpack (replicate 512 42)
        from  = 123
        msg   = M.DATA 0 chunk
    setLastPeer (Just from)
    mock $ ExpectSend from msg
    reply msg
    verify

testSendData = do
    let chunk = bpack (replicate 255 (65 + 10))
        dest = 888
        msg = M.Error M.FileNotFound
    mock $ ExpectSend dest msg
    send 888 msg
    verify

testIncIndex = do
  idx  <- getBlockIndex
  idx' <- incBlockIndex
  assertEqual "incBlockIndex" (idx + 1) idx'

--------------------------------
-- Utilities

-- message IO mock

-- | XFerMock
type XFerMock a = XFerT MessageIOMock Int a

-- | Run some testing code in the XFerT monad that is lifted ontop of a
--   mocked MessageIO monad 'MessageIOMock'
runXFerMock :: XFerMock a -> IO a
runXFerMock action = evalStateT (runXFerT action) []

-- | Specify an expectation.
mock :: Expectation -> XFerMock ()
mock e = lift (modify (++[e]))

-- | Verify that no more expectations exist.
verify :: XFerMock ()
verify = lift $ do
  st <- get
  when (st /= []) (fail $ "missed expectations: " ++ show st)

-- | The expecation type that defines if either a send or a receive is expected.
data Expectation = ExpectSend Int M.Message | ExpectReceive Int M.Message
                   deriving (Eq, Show)

-- | A simple mock monad ontop of IO that is instance of MessageIO
type MessageIOMock = StateT MockSt IO

type MockSt = [Expectation]

instance MessageIO MessageIOMock Int where
    sendTo addr pl = do
      let msg = M.decode pl
      st <- get
      case st of
        [] -> testFail $ printf "no more expectations: sendTo %i %s" addr (show msg)

        (ex : st') -> do
                       put st'
                       assertEqual "sendTo" ex (ExpectSend addr msg)

    receiveFrom _ = do
      st <- get
      case st of
        [] -> testFail "no more expectations: receiveFrom"

        (ExpectReceive addr msg : st') -> do
                      put st'
                      return $ (addr, M.encode msg)

        _ -> testFail "unexpected 'receiveFrom' "

    localAddress = return 0

-- general test util

init_logging = do
  updateGlobalLogger rootLoggerName $ setLevel DEBUG

assertEqual msg a b = do
  let errMsg = printf "assertEqual FAIL: \"%s\"\nexpected: \"%s\"\n\nactual: \"%s\"\n\n"
                msg (show a) (show b)
  when (a /= b) (testFail errMsg)
  return ()

testFail msg = liftIO $ (putStrLn msg >>  exitWith (ExitFailure 1))

l m = liftIO $ infoM "Test" m