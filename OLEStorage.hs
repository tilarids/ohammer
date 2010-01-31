module OLEStorage where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Array.IArray
import Data.Binary.Get as BinaryGet
import Data.Binary.Put as BinaryPut
import Control.Monad.Loops



-- data types ----------------------------------------------
data ByteOrder = LittleEndian | BigEndian | UnknownEndian
  deriving (Show)

data BlockType = MSATType | SATType | SSATType | DirectoryType
  deriving (Show)

data StreamLocation = SATLocation | SSATLocation
  deriving (Show)

type SectorID = Word32

-- master sector allocation table
type MSAT =  [SectorID]

type SAT = Array Int SectorID -- it's an allocation array

-- in fact, it is not only the header. Now it is all the document itself
data Header =
  Header { docId                  :: B.ByteString, -- document id
           uId                    :: B.ByteString, -- unique id
           revision               :: Word16,
           version                :: Word16,
           byteOrder              :: Word16, -- Use ByteOrder in future,
           secSize                :: Word16, -- sector size (usually 512 bytes)
           secSizeShort           :: Word16, -- short sector size (usually 64 bytes)
           numSecSAT              :: Word32, -- total number of sectors in SAT (equals the number of sector IDs
                                             -- stored in the MSAT).
           secIDFirstDirStrm      :: Word32,
           minStreamSize          :: Word32,
           secIDFirstSSAT         :: Word32,
           numSecSSAT             :: Word32,
           secIDFirstMSAT         :: Word32,
           numSecMSAT             :: Word32,
           secIDs                 :: [Word32]
         }
  deriving (Show)

data OLEDocument =
  OLEDocument { header            :: Header,
                bytes             :: B.ByteString
              }
  deriving (Show)

data EntryType = EmptyEntry | UserStorageEntry | UserStreamEntry | LockBytesEntry | ProperyEntry | RootStorageEntry
  deriving (Show)

data EntryNodeColor = RedNode | BlackNode | UnknownNode
  deriving (Show)

data Entry =
  Entry { name                    :: String,
          charBufferSize          :: Int,
          entryType               :: EntryType,
          nodeColor               :: EntryNodeColor,
          uniqueID                :: Int,
          userFlags               :: Int,
          timeCreated             :: Int,
          timeModified            :: Int,
          streamSectorID          :: Int,
          streamSize              :: Int
        }
  deriving (Show)

data Directory =
  Directory { document            :: OLEDocument, -- link to document
              entries             :: [Entry]
            }
  deriving (Show)

-- end of data types -----------------------------------------------
-- instances -------------------------------------------------------
instance Binary Header where
  put = undefined -- no put - we are not saving yet
  get = do docId <- BinaryGet.getLazyByteString 8
           uId <- BinaryGet.getLazyByteString 16
           revision <- BinaryGet.getWord16host
           version <- BinaryGet.getWord16host
           byteOrder <- BinaryGet.getWord16host
           secSize <- BinaryGet.getWord16host
           secSizeShort <- BinaryGet.getWord16host
           BinaryGet.getLazyByteString 10 --unused
           numSecSAT <- BinaryGet.getWord32host
           secIDFirstDirStrm <- BinaryGet.getWord32host
           BinaryGet.getLazyByteString 4 --unused
           minStreamSize <- BinaryGet.getWord32host
           secIDFirstSSAT <- BinaryGet.getWord32host
           numSecSSAT <- BinaryGet.getWord32host
           secIDFirstMSAT <- BinaryGet.getWord32host
           numSecMSAT <- BinaryGet.getWord32host
           secIDs <- (unfoldM (do x <- BinaryGet.getWord32host
                                  return $ if (x == -1) then Nothing else Just x))
           return Header { docId=docId,
                           uId=uId,
                           revision=revision,
                           version=version,
                           byteOrder=byteOrder,
                           secSize=secSize,
                           secSizeShort=secSizeShort,
                           numSecSAT=numSecSAT,
                           secIDFirstDirStrm=secIDFirstDirStrm,
                           minStreamSize=minStreamSize,
                           secIDFirstSSAT=secIDFirstSSAT,
                           numSecSSAT=numSecSSAT,
                           secIDFirstMSAT=secIDFirstMSAT,
                           numSecMSAT=numSecMSAT,
                           secIDs=secIDs
                         }

instance Binary OLEDocument where
  put = undefined
  get = do header <- get
           bytes <- BinaryGet.getRemainingLazyByteString
           return OLEDocument { header=header,
                                bytes=bytes
                              }
-- end of instances -----------------------------------------------------------
-- functions ------------------------------------------------------------------


getDirectory :: OLEDocument -> Directory
getDirectory = undefined


-- getMSAT now disregards MSAT's with num of secIDs > 109
getMSAT :: OLEDocument -> MSAT
getMSAT = secIDs . header -- just use secIDs from Header

-- construct SAT using MSAT and sectors from OLEDocument
-- (get the data from all sectors that are defined in MSAT)
getSAT :: MSAT -> OLEDocument -> SAT
getSAT masterSAT doc = listArray (1, (length sat)) sat
    where sat = concat listOfIDs
          listOfIDs = map parseID masterSAT
          parseID theID = BinaryGet.runGet parseSec (B.drop (fromIntegral (secSize'* theID)) (bytes doc))
          parseSec = sequence (replicate idCount BinaryGet.getWord32host)
--          secSize' :: Word32
          secSize' = 2 ^ (secSize (header doc)) -- it is Word32 as needed by parseID
--          idCount :: Int
          idCount = (fromIntegral secSize') `div` 4 -- it is Int as needed by parseSec
                                                    -- that's why fromIntegral is used

--parseID doc theID = BinaryGet.runGet (parseSecM doc) (B.drop (fromIntegral ((secSize' doc)* theID)) (bytes doc))
--parseSecM doc = sequence (replicate (idCount doc) BinaryGet.getWord32host)
--secSize' doc = 2 ^ (secSize (header doc))
--idCount doc = (secSize' doc) `div` 4

parseHeader :: B.ByteString -> Header
parseHeader = decode

parseDocument :: B.ByteString -> OLEDocument
parseDocument = decode

parseByteOrder :: Word16 -> ByteOrder
parseByteOrder x = undefined
