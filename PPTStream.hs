module PPTStream where

import Data.List
import Data.Maybe
import OLEStorage
import Data.Binary
import Data.Binary.Get as BinaryGet
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Control.Monad.Loops

-- data types -------------------------------------------------------------------------
data PPTRecordHeader = PPTRecordHeader { recVer         :: Int, -- first 4 bits
                                         recInstance    :: Int, -- next 12 bits
                                         recType        :: Word16, -- next 2 bytes, should be converted later
                                         recLen         :: Word32 -- next 4 bytes
                                       }
  deriving (Show)

data PPTNode = PPTContainer { recordHeader                   :: PPTRecordHeader, -- header
                              childrenNodes                  :: [PPTNode]  -- children
                              } |
               PPTAtom      { recordHeader                   :: PPTRecordHeader, -- header
                              nodeBinary                     :: B.ByteString -- binaryData
                            }
  deriving (Show)
-- end of data types ------------------------------------------------------------------
-- instances --------------------------------------------------------------------------
instance Binary PPTRecordHeader where
  put = undefined
  get = do verAndInstance <- BinaryGet.getWord16le
           recType <- BinaryGet.getWord16le
           recLen <- BinaryGet.getWord32le
           return PPTRecordHeader { recVer=fromIntegral $ verAndInstance .&. 0x000f,
                                    recInstance=shiftR 4 (fromIntegral (verAndInstance .&. 0xfff0)), -- TODO: fix
                                    recType=recType,
                                    recLen=recLen
                                  }

readPPTChildren header = unfoldrM f 0
    where f acc
            | acc == recLen header = do return Nothing
            | otherwise = do node <- get
                             return $ Just (node, acc + recLen (recordHeader node) + 8) -- 8 is a length of header

instance Binary PPTNode where
  put = undefined
  get = do recHeader <- get
           if isContainer recHeader
              then do children <- readPPTChildren recHeader
                      return PPTContainer { recordHeader=recHeader,
                                            childrenNodes=children
                                          }
              else do nodeBinary <- BinaryGet.getLazyByteString $ fromIntegral (recLen recHeader)
                      return PPTAtom { recordHeader=recHeader,
                                       nodeBinary=nodeBinary
                                     }
-- end of instances--------------------------------------------------------------------
-- functions --------------------------------------------------------------------------
isContainer :: PPTRecordHeader -> Bool
isContainer header = isCont && (0x0428 /= recType header) -- old (pre SP1) PP2007 bug: RT_RoundTripCustomTableStyles12Atom has rec_ver==0xF
    where isCont = 0xF == recVer header

parsePPTStream :: B.ByteString -> PPTNode
parsePPTStream bs = PPTContainer { recordHeader=rootHeader,
                                   childrenNodes=children
                                 }
    where rootHeader = PPTRecordHeader { recVer=0xF, recInstance=0, recType=0xFFFF,
                                         recLen=fromIntegral (B.length bs)
                                       }
          children = runGet (readPPTChildren rootHeader) bs

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
