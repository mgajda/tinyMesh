{-# LANGUAGE OverloadedStrings #-}
module TinyMesh where

import           Data.Char
import           Control.Monad
import           Control.Applicative
import           System.Environment  (getArgs)
import           System.Posix.Unistd

-- From bytestring module:
import qualified Data.ByteString.Char8      as BS
-- From serialport package - portable serial port handling:
import           System.Hardware.Serialport as Serial
-- From hex package:
import           Data.Hex

serialSettings :: SerialPortSettings
serialSettings  = defaultSerialSettings { commSpeed     = CS19200
                                        , timeout       = 10
                                        , flowControl   = NoFlowControl --Software
                                        , parity        = NoParity
                                        , stopb         = One
                                        , bitsPerWord   = 8
                                        }

responsePacketSize = 128

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

-- Main test script:
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
    putStrLn $ unlines $ map show $ result
