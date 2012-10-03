module DataSpaces where

import Data.List
import Data.Maybe
import OLEStorage
import Data.Binary
import Data.Binary.Get as BinaryGet
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Debug.Trace
-- data types -------------------------------------------------------------------------
data DataSpaceMap = DataSpaceMap { dsmEntries :: [DataSpaceMapEntry] }
  deriving (Show)

data DataSpaceMapEntry = DataSpaceMapEntry { dsmEntryReferenceComponents          :: [DataSpaceReferenceComponent], 
                                             dsmEntryDataSpaceName                :: String
                                           }
  deriving (Show)

data DataSpaceReferenceComponentType = DataSpaceReferenceComponentStream | DataSpaceReferenceComponentStorage
  deriving (Show)

data DataSpaceReferenceComponent = DataSpaceReferenceComponent { dsmRefCompType   :: DataSpaceReferenceComponentType,
                                                                 dsmRefCompString :: String
                                                               }
  deriving (Show)

data UnicodePaddedString = UnicodePaddedString { unicodeValue :: String }
  deriving (Show)

-- end of data types ------------------------------------------------------------------
-- instances --------------------------------------------------------------------------
instance Binary DataSpaceMap where
  put = undefined
  get = do headerLength <- BinaryGet.getWord32le
           if 8 /= (fromIntegral headerLength) then (error $ "headerLength should be equal to 8!" ++ (show headerLength)) else return ()
           entriesCount <- BinaryGet.getWord32le
           entries <- sequence (replicate (fromIntegral entriesCount) get)
           return DataSpaceMap {dsmEntries = entries}

instance Binary DataSpaceMapEntry where
  put = undefined
  get = do dataRecordLength <- BinaryGet.getWord32le
           refCompCount <- BinaryGet.getWord32le
           refComps <- sequence (replicate (fromIntegral refCompCount) get)
           dataSpaceName <- get
           return DataSpaceMapEntry {dsmEntryReferenceComponents = refComps,
                                     dsmEntryDataSpaceName = (unicodeValue dataSpaceName)}

instance Binary DataSpaceReferenceComponentType where
  put = undefined
  get = do refCompType <- BinaryGet.getWord32le
           if 0 == refCompType then return DataSpaceReferenceComponentStream else return DataSpaceReferenceComponentStorage

instance Binary DataSpaceReferenceComponent where
  put = undefined
  get = do refCompType <- get
           refCompString <- get
           return DataSpaceReferenceComponent {dsmRefCompType = refCompType,
                                               dsmRefCompString = unicodeValue refCompString}

instance Binary UnicodePaddedString where
  put = undefined
  get = do dataRecordLength <- BinaryGet.getWord32le
           stringWords <- sequence (replicate ((fromIntegral dataRecordLength) `div` 2) BinaryGet.getWord16le)
           unused <-BinaryGet.getLazyByteString $ ((fromIntegral dataRecordLength) * 2) `mod ` 4
           return UnicodePaddedString {unicodeValue = utf16BytesToString stringWords}

-- end of instances--------------------------------------------------------------------
-- functions --------------------------------------------------------------------------

parseDataSpaceMap :: B.ByteString -> DataSpaceMap
parseDataSpaceMap = decode

dumpCryptoStorage :: B.ByteString -> String -> IO ()
dumpCryptoStorage input dirName = 
    do B.writeFile "/tmp/raw.dump" dsmStream
       putStrLn $ show dsm
    where doc = parseDocument input
          dsmStream = extractEntry doc "DataSpaceMap"
          dsm = parseDataSpaceMap dsmStream
