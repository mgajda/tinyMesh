{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
-- | Defines Parse class and its generic counterpart
-- for easier parsing of packets.
module Packet.Parse where

import           Data.Char
import           GHC.Generics
import           Data.List           (intercalate)
import           Control.Applicative
import           Debug.Trace
--import           System.IO
-- From bytestring module:
import qualified Data.ByteString.Char8      as BS
-- From Attoparsec
import           Data.Attoparsec.ByteString.Char8 as Atto

-- * This class uses generics to give default instance to Parse class.
class GParse a where
  gParser :: Parser (a r)

instance GParse U1 where
  gParser = return U1

instance (Parse a) => GParse (K1 i a) where
  gParser = K1 <$> parser

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
  parser         :: Parser a
  default parser :: (Generic a, GParse (Rep a)) => Parser a
  parser          = to <$> gParser

instance Parse Int where
  parser = ord <$> anyChar

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

-- | WARNING: doesn't seem to work!!!
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
          

