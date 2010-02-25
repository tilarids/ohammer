module PPTStream where

import List
import Maybe
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

data PPTNode = PPTContainer { recordHeader                   :: PPTRecordHeader, -- header
                              childrenNodes                  :: [PPTNode]  -- children
                              } |
               PPTAtom      { recordHeader                   :: PPTRecordHeader, -- header
                              nodeBinary                     :: B.ByteString -- binaryData
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

-- readPPTChildren :: PPTRecordHeader -> Word32 -> Get (Maybe (PPTNode, Word32))
-- readPPTChildren recHeader acc = if acc == (recLen recHeader)
--                                    then Get Nothing
--                                    else do node <- get
--                                            node :: PPTNode
--                                            return Just (node, acc + recLen (recordHeader node))

-- readPPTChildren2 :: PPTRecordHeader -> [Get PPTNode]
-- readPPTChildren2 header = impl 0 []
--     where impl total l
--             | total == (recLen header) = reverse l
--             | otherwise = do node <- get
--                              node :: PPTNode
--                              return (impl (total + recLen (recordHeader node)) (node : l))
--           impl :: Word32 -> [PPTNode] -> [PPTNode]

instance Binary PPTNode where
  put = undefined
  get = do recHeader <- get
           if isContainer recHeader
              then do return PPTContainer { recordHeader=recHeader,
                                            childrenNodes=[]
                                          }
              else do nodeBinary <- BinaryGet.getLazyByteString $ fromIntegral (recLen recHeader)
                      return PPTAtom { recordHeader=recHeader,
                                       nodeBinary=nodeBinary
                                     }
           return PPTContainer {recordHeader=recHeader,
                                childrenNodes=[]}
-- end of instances--------------------------------------------------------------------
-- functions --------------------------------------------------------------------------
isContainer :: PPTRecordHeader -> Bool
isContainer header = isCont && (0x0428 == recType header) -- old (pre SP1) PP2007 bug: RT_RoundTripCustomTableStyles12Atom has rec_ver==0xF
    where isCont = 0xF == recVer header

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
