{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
module TinyMesh where

import           Data.Char
import           GHC.Generics
import           Data.List           (intercalate)
import           Control.Monad
import           Control.Applicative
import           System.Environment  (getArgs)
import           System.Posix.Unistd
import           Debug.Trace
--import           System.IO
-- From bytestring module:
import qualified Data.ByteString.Char8      as BS
-- From serialport package - portable serial port handling:
import           System.Hardware.Serialport as Serial
-- From hex package:
import           Data.Hex
-- From Attoparsec
import           Data.Attoparsec.ByteString.Char8 as Atto

import           Packet.Parse

-- | Serial settings successfully used for communication.
serialSettings :: SerialPortSettings
serialSettings  = defaultSerialSettings { commSpeed     = CS19200
                                        , timeout       = 1
                                        , flowControl   = NoFlowControl --Software
                                        , parity        = NoParity
                                        , stopb         = One
                                        , bitsPerWord   = 8
                                        }

--From datasheet:
--http://tiny-mesh.com/mesh-network/pdf/RCxxxx%28HP%29-TM_Data_Sheet_1_42.pdf
--Query message p. 22:
queryCmd :: BS.ByteString
Right queryCmd = unhex $ BS.concat [
--Checksum:
                              "0A",
--bcast address
                              "FF", "FF", "FF", "FF",
--Any random number below (hex) <80
                              "51",

-- Command code:
-- 11 - list devs | 12 - get directly connected | 16 - get paths
                              "03", "11",
-- packet fill:
                              "00", "00"
                             ]
-- | Reading exactly N bytes from serial port, or reporting a failure.
readN :: SerialPort -> Int -> IO (Maybe BS.ByteString)
readN ser n = reader n []
  where
    finalize = Just . BS.concat . reverse
    reader :: Int -> [BS.ByteString] -> IO (Maybe BS.ByteString)
    reader m _   | m < 0 = error "FIXME: Read too many bytes!!!"
    reader 0 acc         = return $ finalize acc
    reader i acc         = do
      rest <- Serial.recv ser i
      if BS.length rest == 0 then do
        print $ "Nothing in readN" ++ show (finalize acc)
        return Nothing -- not enough bytes:
      else
        reader (i-BS.length rest) (rest:acc)

-- | Reading a packet:
readPacket :: SerialPort -> IO (Maybe BS.ByteString)
readPacket ser = do
    hdr  <- Serial.recv ser 1
    if BS.null hdr then
       return Nothing
    else
       let packetSize = ord $ BS.head hdr
       in do
         rest <- readN ser (packetSize - 1)
         case rest of
           Nothing      -> return   Nothing
           Just payload -> return $ Just $ hdr `BS.append` payload

-- | Read all the packets currently in the buffer/network.
readPackets :: SerialPort -> IO [BS.ByteString]
readPackets ser = reverse <$> reader []
  where
    reader acc = do result <- readPacket ser
                    case result of
                      Nothing  -> return $ reverse acc
                      Just pkt -> reader $     pkt:acc

-- TODO: Use Generic?
data Header = Header {
                len        :: Int     -- ^ length of the packet
              , systemId   :: NetAddr -- ^ system address for mesh nodes
              , originId   :: NetAddr -- ^ network address of originating node
              , originRSSI :: Int     -- ^ origin RSSI
              , netLevel   :: Int     -- ^ number of vertical hops to gateway
              , hops       :: Int     -- ^ number of actual hops from router to gateway
              , origMsgCnt :: Int     -- ^ origin message counter
              , latency    :: Int     -- ^ latency between message creation and delivery *10ms
              , packetType :: Int     -- ^ integer packet type
              }
  deriving(Show, Generic)

netaddr :: Parser NetAddr
netaddr = parser

instance Parse Header where
  parser = Header <$> byte   -- length, byte
                  <*> netaddr
                  <*> netaddr
                  <*> byte   -- originRSSI
                  <*> byte   -- network level
                  <*> byte   -- hops
                  <*> word   -- origin message counter
                  <*> word   -- latency
                  <*> byte   -- packet type
              --                   (BS.length <$> untilEOF anyChar)
              --endOfInput

headerLen :: Int
headerLen =  17 -- bytes

data Payload = Event {
               }
             | Serial {
                 blockCount  :: Maybe Int
               , serData     :: BS.ByteString
               }
             | Unknown BS.ByteString
  deriving Show

data Packet = Packet { 
    header  :: Header
  , payload :: Payload
  } deriving(Show, Generic)

newtype NetAddr = NetAddr { netAddrAsTuple :: (Int, Int, Int, Int) }
  deriving Generic

instance Show NetAddr where
  show (NetAddr (d, c, b, a)) = "." `intercalate` map show [a, b, c, d]

instance Parse NetAddr where
  parser = NetAddr <$> parser

byte :: Parser Int
byte = ord <$> anyChar

word :: Parser Int
word = compute <$> byte <*> byte
  where
    compute a b = a*256+b

-- | TODO: Hex literals
instance Parse Packet where
  parser = do
    hdr <- parser
    let bytesLeft = len hdr - headerLen
    Packet hdr <$> case packetType hdr of
                     2         -> parseEvent
                     16        -> parseSerial bytesLeft
                     otherType -> trace ("Unknown packet type: " ++ show otherType) $
                                    Unknown <$> bytestringParser bytesLeft

parseEvent :: Parser Payload
parseEvent = return Event {}

parseSerial :: Int -> Parser Payload
parseSerial bytesLeft = do
    blockCounter <- nothingIfZero <$> parser
    Serial blockCounter <$> bytestringParser (bytesLeft - 1)
  where
    nothingIfZero 0 = Nothing
    nothingIfZero i = Just i 

-- TODO: more efficient bytestring parsing
bytestringParser :: Int -> Parser BS.ByteString
bytestringParser i = BS.pack <$> count i anyChar

parsePacket :: BS.ByteString -> Packet
parsePacket  = parseBS

-- | Main test script:
main :: IO ()
main = do
  serDevs <- getArgs
  --print queryCmd
  forM_ serDevs $ \serDev -> do
    ser <- openSerial serDev serialSettings
    flush ser
    putStrLn $ "Opened serial on " ++ serDev
    _ <- Serial.send ser queryCmd
    usleep $ 1*1000000
    --Responses (p.24):
    -- 2 on -> 4 bytes with the address
    -- 18 on -> statuses, sensor readings
    result <- readPackets ser
    putStrLn $ unlines $ map show result
    mapM_ (print . parsePacket) result
