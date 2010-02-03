module PPTStream where

import List
import Maybe
import OLEStorage
import Data.Binary
import Data.Binary.Get as BinaryGet
import Data.Bits
import qualified Data.ByteString.Lazy as B

-- data types -------------------------------------------------------------------------
data PPTRecordHeader = PPTRecordHeader { recVer         :: Int, -- first 4 bits
                                         recInstance    :: Int, -- next 12 bits
                                         recType        :: Word16, -- next 2 bytes, should be converted later
                                         recLen         :: Word32 -- next 4 bytes 
                                       }
-- end of data types ------------------------------------------------------------------
-- instances --------------------------------------------------------------------------
instance Binary PPTRecordHeader where
  put = undefined
  get = do verAndInstance <- BinaryGet.getWord16le
           recType <- BinaryGet.getWord16le
           recLen <- BinaryGet.getWord32le
           return PPTRecordHeader { recVer=shiftR 12 (fromIntegral (verAndInstance .&. 0xf000)),
                                    recInstance=fromIntegral $ verAndInstance .&. 0x0fff,
                                    recType=recType,
                                    recLen=recLen
                                  }
-- end of instances--------------------------------------------------------------------
-- functions --------------------------------------------------------------------------
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
