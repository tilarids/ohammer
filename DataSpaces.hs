module DataSpaces where

import Data.List
import Data.Maybe
import OLEStorage
import Data.Binary
import Data.Binary.Get as BinaryGet
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Debug.Trace
import Data.ByteString.Lazy.UTF8 (toString) 

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

data DataSpaceDefinition = DataSpaceDefinition { dsdTransformRefs :: [UnicodePaddedString] }
  deriving (Show)

data UnicodePaddedString = UnicodePaddedString { unicodeValue :: String }
  deriving (Show)

data UTF8PaddedString = UTF8PaddedString { utf8Value :: String }
  deriving (Show)

data TransformDefinition = TransformDefinition { transformInfoHeader        :: TransformInfoHeader, 
                                                 transformEncryptionInfo    :: EncryptionTransformInfo, -- TODO: check if it possible to not have it
                                                 transformBytes             :: B.ByteString
                                               }
  deriving (Show)

data TransformInfoHeader = TransformInfoHeader { transformID                  :: String,
                                                 transformName                :: String,
                                                 transformReaderVersion       :: Version,
                                                 transformUpdaterVersion      :: Version,
                                                 transformWriterVersion       :: Version
                                               }      
  deriving (Show)

data Version = Version { versionMajor       :: Word16,
                         versionMinor       :: Word16
                       }
  deriving (Show)

-- TransformInfoHeader is not included
data IRMDSTransformInfo = IRMDSTransformInfo { irmdsXrMLLicense          :: String
                                               }
  deriving (Show)

data EncryptionVerifier = EncryptionVerifier {
                                                verifierSalt              :: B.ByteString,
                                                verifierString            :: B.ByteString,
                                                verifierHash              :: B.ByteString
                                             }
  deriving (Show)

data EncryptionInfoStream = EncryptionInfoStream { encryptionVersion  :: Version,
                                                   encryptionHeader   :: EncryptionHeader,
                                                   encryptionVerifier :: EncryptionVerifier
                                                 } 
                          | EncryptionInfoStreamXML { encryptionXMLVersion  :: Version,
                                                      encryptionXMLFlags    :: EncryptionHeaderFlags,
                                                      encryptionXML         :: B.ByteString
                                                 }
  deriving (Show)
      
data EncryptionHeader = EncryptionHeader { encryptionFlags          :: EncryptionHeaderFlags,
                                           encryptionAlgID          :: EncryptionAlgorithm,
                                           encryptionAlgIDHash      :: EncryptionHash,
                                           encryptionKeySize        :: Int,
                                           encryptionProvider       :: EncryptionProvider

                                         }
  deriving (Show)

data EncryptionHeaderFlags = EncryptionHeaderFlags { encFlagCryptoAPI         :: Bool,
                                                     encFlagDocProps          :: Bool,
                                                     encFlagExternal          :: Bool,
                                                     encFlagAES               :: Bool
                                                   }
  deriving (Show, Eq)

data EncryptionAlgorithm =  EncryptionAlgorithmCustom 
                          | EncryptionAlgorithmRC4 
                          | EncryptionAlgorithmAES128 
                          | EncryptionAlgorithmAES192
                          | EncryptionAlgorithmAES256
  deriving (Show)

data EncryptionHash =  EncryptionHashCustom | EncryptionHashSHA1
  deriving (Show)

data EncryptionProvider =  EncryptionProviderAny | EncryptionProviderRC4 | EncryptionProviderAES
  deriving (Show)


data EncryptionTransformInfo = EncryptionTransformInfo { encryptionName           :: String,
                                                         encryptionBlockSize      :: Int,
                                                         encryptionCipherMode     :: Word32
                                                       }
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

instance Binary DataSpaceDefinition where
  put = undefined
  get = do headerLength <- BinaryGet.getWord32le
           if 8 /= (fromIntegral headerLength) then (error $ "headerLength should be equal to 8!" ++ (show headerLength)) else return ()
           refsCount <- BinaryGet.getWord32le
           refs <- sequence (replicate (fromIntegral refsCount) get)
           return DataSpaceDefinition {dsdTransformRefs = refs}


instance Binary UnicodePaddedString where
  put = undefined
  get = do dataRecordLength <- BinaryGet.getWord32le
           stringWords <- sequence (replicate ((fromIntegral dataRecordLength) `div` 2) BinaryGet.getWord16le)
           unused <- BinaryGet.getLazyByteString $ padTo 4 $ fromIntegral dataRecordLength
           return UnicodePaddedString {unicodeValue = utf16BytesToString stringWords}

instance Binary UTF8PaddedString where
  put = undefined
  get = do dataRecordLength <- BinaryGet.getWord32le
           s <- BinaryGet.getLazyByteString $ fromIntegral dataRecordLength
           unused <- BinaryGet.getLazyByteString $ padTo 4 $ fromIntegral dataRecordLength
           return UTF8PaddedString {utf8Value = toString s}

instance Binary TransformDefinition where
  put = undefined
  get = do transformHeader <- get
           if "{FF9A3F03-56EF-4613-BDD5-5A41C1D07246}" /= (transformID transformHeader) 
            then error "At the moment only ECMA-376 encryption is supported" 
            else return ()
           --error $ "Got:" ++ (show transformHeader)
           encInfo <- get
           remainingBytes <- BinaryGet.getRemainingLazyByteString
           return TransformDefinition { transformInfoHeader = transformHeader,
                                        transformEncryptionInfo = encInfo,
                                        transformBytes = remainingBytes}

instance Binary TransformInfoHeader where
  put = undefined
  get = do transformLen <- BinaryGet.getWord32le
           transformType <- BinaryGet.getWord32le
           transformIDString <- get
           transformNameString <- get
           transformReaderVer <- get
           transformUpdaterVer <- get
           transformWriterVer <- get
           return TransformInfoHeader { transformID = unicodeValue transformIDString,
                                        transformName = unicodeValue transformNameString,
                                        transformReaderVersion = transformReaderVer,
                                        transformUpdaterVersion = transformUpdaterVer,
                                        transformWriterVersion = transformWriterVer
                                      }

instance Binary Version where
  put = undefined
  get = do vMajor <- BinaryGet.getWord16le
           vMinor <- BinaryGet.getWord16le
           return Version { versionMajor = vMajor,
                            versionMinor = vMinor
                          }

instance Binary IRMDSTransformInfo where
  put = undefined
  get = do extensibilityHeader <- BinaryGet.getWord32le -- unused
           if 4 /= extensibilityHeader 
            then error $ "ExtensibilityHeader should be equal to 4! It's " ++ (show extensibilityHeader)
            else return ()
           license <- get
           return IRMDSTransformInfo { irmdsXrMLLicense = utf8Value license }

instance Binary EncryptionVerifier where
  put = undefined
  get = do saltSize <- BinaryGet.getWord32le
           if 16 /= saltSize 
            then error $ "Salt size should be equal to 16! It's " ++ (show saltSize)
            else return ()
           salt <- BinaryGet.getLazyByteString 16
           verifierString <- BinaryGet.getLazyByteString 16
           verifierHashSize <- BinaryGet.getWord32le
           verifierHash <- BinaryGet.getLazyByteString $ fromIntegral verifierHashSize
           return EncryptionVerifier {
                                        verifierSalt = salt,
                                        verifierString = verifierString,
                                        verifierHash = verifierHash 
                                     }

parseBinaryEncryptionInfoStream version flags = 
      do headerSize <- BinaryGet.getWord32le
         encHeader <- get
         if flags /= (encryptionFlags encHeader) 
          then error $ "Flags should be equal! They are " ++ (show flags) ++ " and " ++ (show $ encryptionFlags encHeader)
          else return ()
         if 32 < headerSize
          then BinaryGet.skip $ fromIntegral (headerSize - 32)
          else return ()
         verifier <- get
         return EncryptionInfoStream { encryptionVersion = version,
                                       encryptionHeader = encHeader,
                                       encryptionVerifier = verifier
                                     }

parseXMLEncryptionInfoStream version flags= 
      do xml <- BinaryGet.getRemainingLazyByteString
         return EncryptionInfoStreamXML {
                                          encryptionXMLVersion = version,
                                          encryptionXMLFlags = flags,
                                          encryptionXML = xml
                                        }
instance Binary EncryptionInfoStream where
  put = undefined
  get = do version <- get
           flags <- get
           if (4 == (versionMajor version)) && (4 == (versionMinor version))
            then parseXMLEncryptionInfoStream version flags
            else parseBinaryEncryptionInfoStream version flags

instance Binary EncryptionHeader where
  put = undefined
  get = do flags <- get
           sizeExtra <- BinaryGet.getWord32le
           if 0 /= sizeExtra
            then error $ "sizeExtra should be equal to 0! It's " ++ (show sizeExtra)
            else return ()
           algID <- get
           algIDHash <- get
           keySize <- BinaryGet.getWord32le
           provider <- get
           reserved1 <- BinaryGet.getWord32le
           reserved2 <- BinaryGet.getWord32le
           if 0 /= reserved2
            then error $ "reserved2 should be equal to 0! It's " ++ (show reserved2)
            else return ()
           -- WARNING: CSP Name should be here
           return EncryptionHeader { encryptionFlags = flags,
                                     encryptionAlgID = algID,
                                     encryptionAlgIDHash = algIDHash,
                                     encryptionKeySize = fromIntegral keySize,
                                     encryptionProvider = provider
                                   }

instance Binary EncryptionHeaderFlags where
  put = undefined
  get = do flags <- BinaryGet.getWord32le
           return EncryptionHeaderFlags { encFlagCryptoAPI    = 0 /= (flags .&. 0x04),
                                          encFlagDocProps     = 0 /= (flags .&. 0x08),
                                          encFlagExternal     = 0 /= (flags .&. 0x10),
                                          encFlagAES          = 0 /= (flags .&. 0x20)
                                        }

instance Binary EncryptionAlgorithm where
  put = undefined
  get = do algId <- BinaryGet.getWord32le
           return $ mapEncryptionAlgorithm algId

instance Binary EncryptionHash where
  put = undefined
  get = do algIdHash <- BinaryGet.getWord32le
           return $ mapEncryptionHash algIdHash

instance Binary EncryptionProvider where
  put = undefined
  get = do providerType <- BinaryGet.getWord32le
           return $ mapEncryptionProvider providerType

instance Binary EncryptionTransformInfo where
  put = undefined
  get = do encName <- get
           encBlockSize <- BinaryGet.getWord32le
           encCipherMode <- BinaryGet.getWord32le
           reserved <- BinaryGet.getWord32le
           if 4 /= reserved
            then error $ "Reserved field in EncryptionTransformInfo should be equal to 4! It's " ++ (show reserved)
            else return ()
           return EncryptionTransformInfo { encryptionName = utf8Value encName,
                                            encryptionBlockSize = fromIntegral encBlockSize,
                                            encryptionCipherMode = encCipherMode
                                          }

-- end of instances--------------------------------------------------------------------
-- functions --------------------------------------------------------------------------

mapEncryptionAlgorithm :: Word32 -> EncryptionAlgorithm
mapEncryptionAlgorithm 0x0000 = EncryptionAlgorithmCustom
mapEncryptionAlgorithm 0x6801 = EncryptionAlgorithmRC4
mapEncryptionAlgorithm 0x660E = EncryptionAlgorithmAES128
mapEncryptionAlgorithm 0x660F = EncryptionAlgorithmAES192
mapEncryptionAlgorithm 0x6610 = EncryptionAlgorithmAES256
mapEncryptionAlgorithm _ = error "Incorrect value for AlgID"

mapEncryptionHash :: Word32 -> EncryptionHash
mapEncryptionHash 0x0000 = EncryptionHashCustom
mapEncryptionHash 0x8004 = EncryptionHashSHA1
mapEncryptionHash _ = error "Incorrect value for AlgIDHash"

mapEncryptionProvider :: Word32 -> EncryptionProvider
mapEncryptionProvider 0x0000 = EncryptionProviderAny
mapEncryptionProvider 0x0001 = EncryptionProviderRC4
mapEncryptionProvider 0x0018 = EncryptionProviderAES
mapEncryptionProvider _ = error "Incorrect value for ProviderType"

parseDataSpaceMap :: B.ByteString -> DataSpaceMap
parseDataSpaceMap = decode

parseDataSpaceDefinition :: B.ByteString -> DataSpaceDefinition
parseDataSpaceDefinition = decode

parseTransform :: B.ByteString -> TransformDefinition
parseTransform = decode

parseEncryptionInfoStream :: B.ByteString -> EncryptionInfoStream
parseEncryptionInfoStream = decode

padTo padding x 
  | x `mod` padding == 0 = 0
  | otherwise            = padding - (x `mod` padding)


dumpCryptoStorage :: B.ByteString -> String -> IO ()
dumpCryptoStorage input dirName = 
    do B.writeFile "/tmp/raw.dump" dsmStream
       putStrLn $ show dsm
       putStrLn $ show dsds
       putStrLn $ show transform
       putStrLn $ show encryptionInfoStream

    where doc = parseDocument input
          dsmStream = extractEntry doc "DataSpaceMap"
          dsm = parseDataSpaceMap dsmStream
          dsds = map (\x -> parseDataSpaceDefinition (extractEntry doc $ dsmEntryDataSpaceName x)) $ dsmEntries dsm 
          primary = extractEntry doc "\ACKPrimary" -- only one primary st is used
          encryptionInfo = extractEntry doc "EncryptionInfo"
          transform = parseTransform primary
          encryptionInfoStream = parseEncryptionInfoStream encryptionInfo
