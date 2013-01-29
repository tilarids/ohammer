module OLEStorage where

import GHC.Base
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import Data.Binary
import Data.Maybe
import Data.List
import Data.Array.IArray
import Data.Binary.Get as BinaryGet
import Data.Binary.Put as BinaryPut
import System.FilePath.Posix

import Debug.Trace


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

type SAT = Array SectorID SectorID -- it's an allocation array

type SSAT = B.ByteString

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
  deriving (Show, Eq)

data EntryNodeColor = RedNode | BlackNode | UnknownNode
  deriving (Show)

data Entry =
  Entry { entryName               :: String,
          charBufferSize          :: Word16, -- should be ignored - TODO
          entryType               :: EntryType,
          nodeColor               :: EntryNodeColor,
          dirIDLeft               :: Word32,
          dirIDRight              :: Word32,
          dirIDRoot               :: Word32,
          uniqueID                :: B.ByteString,
          userFlags               :: Word32,
          timeCreated             :: Word64,
          timeModified            :: Word64,
          streamSectorID          :: SectorID,
          streamSize              :: Word32
        }
  deriving (Show)

data Directory =
  Directory { document            :: OLEDocument, -- link to document
              entries             :: [Entry]
            }
  deriving (Show)

-- end of data types -----------------------------------------------
-- instances -------------------------------------------------------

-- we should use little endian by default, I think
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
           allSecIDs <- sequence (replicate 109 BinaryGet.getWord32host)

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
                           secIDs=takeWhile (/= -1) allSecIDs
                         }

instance Binary OLEDocument where
  put = undefined
  get = do header <- get
           bytes <- BinaryGet.getRemainingLazyByteString
           return OLEDocument { header=header,
                                bytes=bytes
                              }
instance Binary Entry where
  put = undefined
  get = do name <- sequence (replicate 32 BinaryGet.getWord16host)
           charBufferSize <- BinaryGet.getWord16host
           entryType <- BinaryGet.getWord8
           nodeColor <- BinaryGet.getWord8
           dirIDLeft <- BinaryGet.getWord32host
           dirIDRight <- BinaryGet.getWord32host
           dirIDRoot <- BinaryGet.getWord32host
           uniqueID <- BinaryGet.getLazyByteString 16
           userFlags <- BinaryGet.getWord32host
           timeCreated <- BinaryGet.getWord64host
           timeModified <- BinaryGet.getWord64host
           streamSectorID <- BinaryGet.getWord32host
           streamSize <- BinaryGet.getWord32host
           BinaryGet.getLazyByteString 4 -- unused
           return Entry { entryName=utf16BytesToString name,
                          charBufferSize=charBufferSize,
                          entryType=parseEntryType entryType,
                          nodeColor=parseNodeColor nodeColor,
                          dirIDLeft=dirIDLeft,
                          dirIDRight=dirIDRight,
                          dirIDRoot=dirIDRoot,
                          uniqueID=uniqueID,
                          userFlags=userFlags,
                          timeCreated=timeCreated,
                          timeModified=timeModified,
                          streamSectorID=streamSectorID,
                          streamSize=streamSize
                        }

-- end of instances -----------------------------------------------------------
-- functions ------------------------------------------------------------------

parseEntryType :: Word8 -> EntryType
parseEntryType 0 = EmptyEntry
parseEntryType 1 = UserStorageEntry
parseEntryType 2 = UserStreamEntry
parseEntryType 3 = LockBytesEntry
parseEntryType 4 = ProperyEntry
parseEntryType 5 = RootStorageEntry
parseEntryType x = error $ "Got " ++ (show x) ++ ". Can't parse"

parseNodeColor :: Word8 -> EntryNodeColor
parseNodeColor 0 = RedNode
parseNodeColor 1 = BlackNode
parseNodeColor x = error $ "Got " ++ (show x) ++ ". Can't parse"

utf16BytesToString :: [Word16] -> String
utf16BytesToString from = takeWhile (/= '\NUL') $ map unsafeChr (fromUTF16 (map fromIntegral from))
    where fromUTF16 :: [Int] -> [Int]
          fromUTF16 (c1:c2:wcs)
            | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
              ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
          fromUTF16 (c:wcs) = c : fromUTF16 wcs
          fromUTF16 [] = []

-- construct chain starting with startSector
getChain :: OLEDocument -> SAT -> SectorID -> [SectorID]
getChain doc sat startSector = startSector : buildChain startSector
   where buildChain curSec | curSec > (snd $ bounds sat) = error $ "Got ID " ++ (show curSec) ++ " that's not in SAT:" ++ (show sat)
         buildChain curSec | sat ! curSec == -1 = error "Got -1 as SecID. -1 indicates a free cell"
         buildChain curSec | sat ! curSec == -2 = []
         buildChain curSec | sat ! curSec == -3 = error "Got -3 as SecID. -3 indicates a cell is used by SAT"
         buildChain curSec | sat ! curSec == -4 = error "Got -4 as SecID. -4 indicates a cell is used by MSAT"
         buildChain curSec = newSec : buildChain newSec
             where newSec = sat ! curSec

getSATChain doc startSector = getChain doc sat startSector
    where sat = getSAT msat doc
          msat = getMSAT doc

getSSATChain doc startSector = getChain doc (getSSAT doc) startSector

getSATChainedBytes :: [SectorID] -> OLEDocument -> B.ByteString
getSATChainedBytes chain doc = B.concat byteStrings
    where byteStrings = map getStr chain
          getStr :: SectorID -> B.ByteString
          getStr theID = B.take secSize' (B.drop (secSize' * (fromIntegral theID)) (bytes doc))
          secSize' = 2 ^ (secSize (header doc))

getSSATChainedBytes :: [SectorID] -> OLEDocument -> B.ByteString
getSSATChainedBytes chain doc = B.concat byteStrings
    where byteStrings = map getStr chain
          getStr :: SectorID -> B.ByteString
          getStr theID = B.take secSize' $ B.drop (secSize' * (fromIntegral theID)) (getShortStreamContainerBytes doc)
          secSize' = 2 ^ (secSizeShort (header doc))

getShortStreamContainerBytes :: OLEDocument -> B.ByteString
getShortStreamContainerBytes doc = getSATChainedBytes chain doc
    where dir = getDirectory doc
          chain = getSATChain doc $ streamSectorID (head $ entries dir)

parseEntries :: B.ByteString ->  [Entry]
parseEntries bytes = BinaryGet.runGet parseSec bytes
    where parseSec = sequence (replicate entriesCount get)
          entriesCount = fromIntegral $ (B.length bytes) `div` 128

getDirectory :: OLEDocument -> Directory
getDirectory doc = Directory doc entries
    where dirID = secIDFirstDirStrm $ header doc
          chain = getSATChain doc dirID
          chainedBytes = getSATChainedBytes chain doc
          entries = parseEntries chainedBytes

-- getMSAT now disregards MSAT's with num of secIDs > 109
getMSAT :: OLEDocument -> MSAT
getMSAT = secIDs . header -- just use secIDs from Header

getSSAT :: OLEDocument -> SAT
getSSAT doc = listArray (0, (fromIntegral (length ssat)) - 1) ssat
    where ssatId = secIDFirstSSAT (header doc)
          chain = getSATChain doc ssatId
          ssatBytes = getSATChainedBytes chain doc
          ssat :: [Word32]
          ssat = BinaryGet.runGet parseSec ssatBytes
          parseSec = sequence (replicate idCount BinaryGet.getWord32le)
          secSizeShort' :: Word32
          secSizeShort' = 2 ^ ((secSizeShort (header doc)) + 1) -- it is Word32 as needed by parseID
          idCount :: Int
          idCount = (fromIntegral secSizeShort') `div` 4 -- it is Int as needed by parseSec
                                                         -- that's why fromIntegral is used

getEntryBytes :: OLEDocument -> Entry -> B.ByteString
getEntryBytes doc entry 
    |    (entryType entry /= RootStorageEntry) 
      && (streamSize entry) < (minStreamSize (header doc)) = getSSATChainedBytes chain doc
  where chain = getSSATChain doc (streamSectorID entry)
getEntryBytes doc entry = getSATChainedBytes chain doc
  where chain = getSATChain doc (streamSectorID entry)


-- construct SAT using MSAT and sectors from OLEDocument
-- (get the data from all sectors that are defined in MSAT)
getSAT :: MSAT -> OLEDocument -> SAT
getSAT masterSAT doc = listArray (0, (fromIntegral (length sat)) - 1) sat
    where sat = concat listOfIDs
          listOfIDs = map parseID masterSAT
          parseID theID = BinaryGet.runGet parseSec (B.drop (fromIntegral (secSize'* theID)) (bytes doc))
          parseSec = sequence (replicate idCount BinaryGet.getWord32host)
          secSize' :: Word32
          secSize' = 2 ^ (secSize (header doc)) -- it is Word32 as needed by parseID
          idCount :: Int
          idCount = (fromIntegral secSize') `div` 4 -- it is Int as needed by parseSec
                                                    -- that's why fromIntegral is used

parseHeader :: B.ByteString -> Header
parseHeader = decode

parseDocument :: B.ByteString -> OLEDocument
parseDocument = decode

parseByteOrder :: Word16 -> ByteOrder
parseByteOrder x = undefined

dumpDocument :: OLEDocument -> String -> IO ()
dumpDocument doc dirName = mapM_ dumpEntry $ entries dir
    where dir = getDirectory doc
          dumpEntry entry | (entryType entry == EmptyEntry) = return ()
          dumpEntry entry = do
              let fname = (dumpName entry)
              putStr $ "Dumping " ++ fname ++ " (" ++ (show $ streamSize entry) ++ " bytes)..."
              B.writeFile fname (getEntryBytes doc entry)
              putStrLn "Done!"
          dumpName entry = combine dirName (entryName entry)

dumpOLEStorage :: B.ByteString -> String -> IO ()
dumpOLEStorage input dirName = dumpDocument doc dirName
     where doc = parseDocument input

extractEntry doc name = B.take (fromIntegral (streamSize entry)) (getEntryBytes doc entry)
    where dir = getDirectory doc
          entry = fromJust $ find (\x -> (entryName x) == name) (entries dir)

extractFileEntry file name = extractEntry doc name
    where doc = parseDocument file
