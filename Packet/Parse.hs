{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts  #-}
-- | Defines Parse class and its generic counterpart
-- for easier parsing of packets.
module Packet.Parse where

import           Data.Char
--import           Data.List           (intercalate)
import           Control.Applicative
import           Debug.Trace
--import           System.IO
-- From bytestring module:
import qualified Data.ByteString.Char8      as BS
-- From Attoparsec
import           Data.Attoparsec.ByteString.Char8 as Atto

-- | Class of things that have a default parsing from ByteString.
class Parse a where
  parser         :: Parser a
  --default parser :: (Generic a, GParse (Rep a)) => Parser a
  --parser          = to <$> gParser

instance Parse Int where
  parser = ord <$> anyChar

instance (Parse a
         ,Parse b) => Parse (a, b) where
  parser = (,) <$> parser
               <*> parser

instance (Parse a
         ,Parse b
         ,Parse c) => Parse (a, b, c) where
  parser = (,,) <$> parser
                <*> parser
                <*> parser

instance (Parse a
         ,Parse b
         ,Parse c
         ,Parse d) => Parse (a, b, c, d) where
  parser = (,,,) <$> parser
                 <*> parser
                 <*> parser
                 <*> parser

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
          

