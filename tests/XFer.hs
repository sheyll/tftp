module Main where

import Network.TFTP.XFer
import Network.TFTP.Types
import qualified Network.TFTP.Message as M
import System.IO(stdout)

import Debug.Trace

main = do
  init_logging
  memIOXFer $ do
         testReceive
         testReplyData

testReceive = do
  let m = M.RRQ "test.txt" M.NetASCII
  fakeIncoming 777 m
  m' <- receive
  peer <- getLastPeer
  assertEqual "testReceive wrong peer" peer (Just 777)
  assertEqual "testReceive invalid mesage" m' m

testReplyData = do
    let chunk = bpack (replicate 512 65)
    setLastPeer (Just 777)
    replyData chunk
    assertSent 777 (M.DATA 1 chunk)

---------------------------------------------------------------------
--- MemIO variant of MessageIO

type MemIOXFer a = XFerT MemIO Int a
type MemIO = StateT MemIOSt IO

memIOXFer :: MemIOXFer a -> IO a
memIOXFer action =
    evalStateT (runXFerT action) (MemIOSt Nothing Nothing [])

data MemIOSt = MemIOSt { lastDest :: Maybe Int
                       , lastMsg  :: Maybe ByteString
                       , inbox    :: [(Int, ByteString)]
                       }

inboxAdd :: (Int, ByteString) -> MemIOSt -> MemIOSt
inboxAdd msg st = st { inbox = (msg:inbox st)}

instance MessageIO MemIO Int ByteString where
    sendTo addr pl = do
      let pl' = M.decode pl
      modify (\st -> st { lastDest = Just addr
                        , lastMsg  = Just pl' })

    receiveFrom _ = do
      (msg:rest) <- inbox <$> get
      modify $ \st -> st { inbox = rest }
      return msg

assertSent :: Int -> M.Message -> MemIOXFer ()
assertSent addr msg = do
  memSt <- lift get
  assertEqual "assertSent last dest" (Just addr) (lastDest memSt)
  assertEqual "assertSent last message"  (Just $ M.encode msg) (lastMsg memSt)

fakeIncoming :: Int -> M.Message -> MemIOXFer ()
fakeIncoming from msg =
    lift $ modify (inboxAdd (from, M.encode msg))

init_logging = do
  h <- streamHandler stdout DEBUG
  updateGlobalLogger rootLoggerName
                         ( setLevel DEBUG
                           . addHandler h)


assertEqual msg a b = do
  when (a /= b) (error $ printf "assertEqual FAIL: \"%s\"\nexpected: \"%s\"\n\nactual: \"%s\"\n\n" msg (show a) (show b))
  return ()