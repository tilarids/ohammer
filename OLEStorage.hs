module OLEStorage where

import Data.ByteString
import Data.Binary
import Data.Binary.Get as BinaryGet
import Data.Binary.Put as BinaryPut

data ByteOrder = LittleEndian | BigEndian | UnknownEndian

data BlockType = MSATType | SATType | SSATType | DirectoryType

data StreamLocation = SATLocation | SSATLocation

data MSAT =
  MSAT { sectorSize               :: Int,
         secIDs                   :: [Int]
       }

data Header =
  Header { docId                  :: ByteString, -- document id
           uId                    :: ByteString, -- unique id
           revision               :: Word16,
           version                :: Word16,
           byteOrder              :: ByteOrder,
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
           masterSAT              :: MSAT -- master sector allocation table
         }

parseByteOrder :: Word16 -> ByteOrder
parseByteOrder x = undefined

instance Binary Header where
  put = undefined -- no put - we are not saving yet
  get = do docId <- BinaryGet.getByteString 8
           uId <- BinaryGet.getByteString 16
           revision <- BinaryGet.getWord16host
           version <- BinaryGet.getWord16host
           byteOrder <- BinaryGet.getWord16host
           secSize <- BinaryGet.getWord16host
           secSizeShort <- BinaryGet.getWord16host
           BinaryGet.getByteString 10 --unused
           numSecSAT <- BinaryGet.getWord32host
           secIDFirstDirStrm <- BinaryGet.getWord32host
           minStreamSize <- BinaryGet.getWord32host
           secIDFirstSSAT <- BinaryGet.getWord32host
           numSecSSAT <- BinaryGet.getWord32host
           secIDFirstMSAT <- BinaryGet.getWord32host
           numSecMSAT <- BinaryGet.getWord32host
           let masterSAT = MSAT {sectorSize=0, secIDs=[]} -- dummy masterSAT
           return Header {
                           docId=docId,
                           uId=uId,
                           revision=revision,
                           version=version,
                           byteOrder=(parseByteOrder byteOrder),
                           secSize=secSize,
                           secSizeShort=secSizeShort,
                           numSecSAT=numSecSAT,
                           secIDFirstDirStrm=secIDFirstDirStrm,
                           minStreamSize=minStreamSize,
                           secIDFirstSSAT=secIDFirstSSAT,
                           numSecSSAT=numSecSSAT,
                           secIDFirstMSAT=secIDFirstMSAT,
                           numSecMSAT=numSecMSAT,
                           masterSAT=masterSAT
                         }
