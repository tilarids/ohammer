module PPTStream where

import List
import Maybe
import OLEStorage
import qualified Data.ByteString.Lazy as B

parsePPTStream = parseDocument

streamHeaderInfo :: OLEDocument -> String
streamHeaderInfo (OLEDocument header _) = show header

streamDirectoryInfo :: OLEDocument -> String
streamDirectoryInfo doc = show (entries dir)
    where dir = getDirectory doc

extractEntry file name = B.take (fromIntegral (streamSize entry)) (getBytes entry)
    where doc = parseDocument file
          dir = getDirectory doc
          entry = fromJust $ find (\x -> (entryName x) == name) (entries dir)
          getBytes entry = getChainedBytes (chain entry) doc
          chain entry = getChain doc (streamSectorID entry)
