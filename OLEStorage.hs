module OLEStorage where

import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Binary.Get as BinaryGet
import Data.Binary.Put as BinaryPut
import Control.Monad.Loops


data ByteOrder = LittleEndian | BigEndian | UnknownEndian
  deriving (Show)

data BlockType = MSATType | SATType | SSATType | DirectoryType
  deriving (Show)

data StreamLocation = SATLocation | SSATLocation
  deriving (Show)

-- master sector allocation table
data MSAT =
  MSAT { sectorSize               :: Int,
         masterSecIDs                   :: [Word32]
       }
  deriving (Show)

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

parseByteOrder :: Word16 -> ByteOrder
parseByteOrder x = undefined

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

parseHeader :: B.ByteString -> Header
parseHeader = decode 

parseDocument :: B.ByteString -> OLEDocument
parseDocument = decode
