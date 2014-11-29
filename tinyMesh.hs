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
--Reading exactly N bytes from serial port, or reporting a failure.

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
-- Reading a packet:

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
data Packet = Packet {
                len        :: Int     -- ^ length of the packet
              , systemId   :: NetAddr -- ^ system address for mesh nodes
              , originId   :: NetAddr -- ^ network address of originating node
              , originRSSI :: Int     -- ^ origin RSSI
              , hops       :: Int     -- ^ number of vertical hops to gateway
              , origMsgCnt :: Int     -- ^ origin message counter
              --, leftover :: BS.ByteString
              }
  deriving(Show, Generic)

class GParse a where
  gParser :: Parser (a r)

instance GParse U1 where
  gParser = return U1

instance (Parse a) => GParse (K1 i a) where
  gParser = do k <- parser
               return $ K1 k

instance (GParse f, GParse g) => GParse (f :*: g) where
  gParser = (:*:) <$> gParser <*> gParser

instance (GParse f, GParse g) => GParse (f :+: g) where
  gParser = (L1 <$> gParser) <|> (R1 <$> gParser)

{-
instance (GParse a, GParse b) => GParse (a :*: b) where
  gParser :: Parser (
  gParser (a :*: b) = (:*:) <$> gParser a
                            <*> gParser b-}

-- | Class of things that have a default parsing from ByteString.
class Parse a where
  parser :: Parser a
  default parser :: (Generic a, GParse (Rep a)) => Parser a
  parser = to <$> gParser

-- | Parse single byte
byte :: Parser Int
byte = ord <$> anyChar

instance Parse Int where
  parser = byte

newtype NetAddr = NetAddr { netAddrAsTuple :: (Int, Int, Int, Int) }

instance Show NetAddr where
  show (NetAddr (d, c, b, a)) = "." `intercalate` map show [a, b, c, d]

instance Parse NetAddr where
  parser = NetAddr <$> ((,,,) <$> parser <*> parser <*> parser <*> parser)

instance Parse Packet where
  parser =    Packet <$> byte
                     <*> parser
                     <*> parser
                     <*> parser
                     <*> parser
                     <*> parser
              --                   (BS.length <$> untilEOF anyChar)
              --endOfInput


-- | Parse ByteString to any value that has Parse instance.
parseBS :: (Parse a) => BS.ByteString -> a
parseBS bs = case parse parser bs of
               Done i r        -> if not $ BS.null i then
                                    trace ("Leftover input: " ++ show  i ++
                                           " of length "      ++ show (BS.length i)) r
                                  else
                                    r
               Partial _       -> error $ "Not enough input to parse anything:\n" ++ show bs
               Fail i ctxs msg -> error $ "ParseError: "  ++ msg ++ "\n" ++ show ctxs ++ "\nat:\n" ++
                                          show i


untilEOF :: Parser a -> Parser [a]
untilEOF p = loop []
  where
    loop acc = do
      isEnd <- atEnd
      if isEnd
        then return $ reverse acc
        else do
          n <- p
          loop $ n:acc
          

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
