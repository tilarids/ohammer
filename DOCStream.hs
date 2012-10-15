module DOCStream  where

import Data.Binary
import Data.Binary.Get as BinaryGet
import qualified Data.ByteString.Lazy as B
import Data.Array

-- data types -------------------------------------------------------------------------
type CharPos = Word32 -- Character Position or CP

-- File Information Block Base or FibBase
data FileInfoBlockBase = FileInfoBlockBase { wIdent         :: Word16,
                                             nFib           :: Word16,
                                             lid            :: Word16,
                                             pnNext         :: Word16,
                                             fibBaseFlags1  :: Word16, -- a lot of flags, should be parsed to bits
                                             lKey           :: Word32,
                                             fibBaseFlags2  :: Word16 -- a lot of flags, should be parsed to bits
                                           }
  deriving (Show)

-- FibRgW97
data FileInfoBlockRgW = FileInfoBlockRgW { lidFE            :: Word16
                                         }
  deriving (Show)

-- FibRgLw97
data FileInfoBlockRgLw = FileInfoBlockRgLw { cbMac          :: Word32,
                                             ccpText        :: Word32,
                                             ccpFtn         :: Word32,
                                             ccpHdd         :: Word32,
                                             ccpAtn         :: Word32,
                                             ccpEdn         :: Word32,
                                             ccpTxbx        :: Word32,
                                             ccpHdrTxBx     :: Word32
                                           }
  deriving (Show)

-- FibRgFcLcb

-- fcStshfOrig = 0, lcbStshfOrig = 1, fcStshf = 2, lcbStshf = 3, fcPlcffndRef = 4, lcbPlcffndRef = 5, fcPlcffndTxt = 6, lcbPlcffndTxt = 7, fcPlcfandRef = 8, lcbPlcfandRef = 9, fcPlcfandTxt = 10,
-- lcbPlcfandTxt = 11, fcPlcfSed = 12, lcbPlcfSed = 13, fcPlcPad = 14, lcbPlcPad = 15, fcPlcfPhe = 16, lcbPlcfPhe = 17, fcSttbfGlsy = 18, lcbSttbfGlsy = 19, fcPlcfGlsy = 20, lcbPlcfGlsy = 21,
-- fcPlcfHdd = 22, lcbPlcfHdd = 23, fcPlcfBteChpx = 24, lcbPlcfBteChpx = 25, fcPlcfBtePapx = 26, lcbPlcfBtePapx = 27, fcPlcfSea = 28, lcbPlcfSea = 29, fcSttbfFfn = 30, lcbSttbfFfn = 31,
-- fcPlcfFldMom = 32, lcbPlcfFldMom = 33, fcPlcfFldHdr = 34, lcbPlcfFldHdr = 35, fcPlcfFldFtn = 36, lcbPlcfFldFtn = 37, fcPlcfFldAtn = 38, lcbPlcfFldAtn = 39, fcPlcfFldMcr = 40, lcbPlcfFldMcr = 41,
-- fcSttbfBkmk = 42, lcbSttbfBkmk = 43, fcPlcfBkf = 44, lcbPlcfBkf = 45, fcPlcfBkl = 46, lcbPlcfBkl = 47, fcCmds = 48, lcbCmds = 49, fcUnused1 = 50, lcbUnused1 = 51, fcSttbfMcr = 52, lcbSttbfMcr = 53,
-- fcPrDrvr = 54, lcbPrDrvr = 55, fcPrEnvPort = 56, lcbPrEnvPort = 57, fcPrEnvLand = 58, lcbPrEnvLand = 59, fcWss = 60, lcbWss = 61, fcDop = 62, lcbDop = 63, fcSttbfAssoc = 64, lcbSttbfAssoc = 65,
-- fcClx = 66, lcbClx = 67, fcPlcfPgdFtn = 68, lcbPlcfPgdFtn = 69, fcAutosaveSource = 70, lcbAutosaveSource = 71, fcGrpXstAtnOwners = 72, lcbGrpXstAtnOwners = 73, fcSttbfAtnBkmk = 74, lcbSttbfAtnBkmk = 75,
-- fcUnused2 = 76, lcbUnused2 = 77, fcUnused3 = 78, lcbUnused3 = 79, fcPlcSpaMom = 80, lcbPlcSpaMom = 81, fcPlcSpaHdr = 82, lcbPlcSpaHdr = 83, fcPlcfAtnBkf = 84, lcbPlcfAtnBkf = 85, fcPlcfAtnBkl = 86,
-- lcbPlcfAtnBkl = 87, fcPms = 88, lcbPms = 89, fcFormFldSttbs = 90, lcbFormFldSttbs = 91, fcPlcfendRef = 92, lcbPlcfendRef = 93, fcPlcfendTxt = 94, lcbPlcfendTxt = 95, fcPlcfFldEdn = 96, lcbPlcfFldEdn = 97,
-- fcUnused4 = 98, lcbUnused4 = 99, fcDggInfo = 100, lcbDggInfo = 101, fcSttbfRMark = 102, lcbSttbfRMark = 103, fcSttbfCaption = 104, lcbSttbfCaption = 105, fcSttbfAutoCaption = 106, lcbSttbfAutoCaption = 107,
-- fcPlcfWkb = 108, lcbPlcfWkb = 109, fcPlcfSpl = 110, lcbPlcfSpl = 111, fcPlcftxbxTxt = 112, lcbPlcftxbxTxt = 113, fcPlcfFldTxbx = 114, lcbPlcfFldTxbx = 115, fcPlcfHdrtxbxTxt = 116, lcbPlcfHdrtxbxTxt = 117,
-- fcPlcffldHdrTxbx = 118, lcbPlcffldHdrTxbx = 119, fcStwUser = 120, lcbStwUser = 121, fcSttbTtmbd = 122, lcbSttbTtmbd = 123, fcCookieData = 124, lcbCookieData = 125, fcPgdMotherOldOld = 126,
-- lcbPgdMotherOldOld = 127, fcBkdMotherOldOld = 128, lcbBkdMotherOldOld = 129, fcPgdFtnOldOld = 130, lcbPgdFtnOldOld = 131, fcBkdFtnOldOld = 132, lcbBkdFtnOldOld = 133, fcPgdEdnOldOld = 134,
-- lcbPgdEdnOldOld = 135, fcBkdEdnOldOld = 136, lcbBkdEdnOldOld = 137, fcSttbfIntlFld = 138, lcbSttbfIntlFld = 139, fcRouteSlip = 140, lcbRouteSlip = 141, fcSttbSavedBy = 142, lcbSttbSavedBy = 143,
-- fcSttbFnm = 144, lcbSttbFnm = 145, fcPlfLst = 146, lcbPlfLst = 147, fcPlfLfo = 148, lcbPlfLfo = 149, fcPlcfTxbxBkd = 150, lcbPlcfTxbxBkd = 151, fcPlcfTxbxHdrBkd = 152, lcbPlcfTxbxHdrBkd = 153,
-- fcDocUndoWord9 = 154, lcbDocUndoWord9 = 155, fcRgbUse = 156, lcbRgbUse = 157, fcUsp = 158, lcbUsp = 159, fcUskf = 160, lcbUskf = 161, fcPlcupcRgbUse = 162, lcbPlcupcRgbUse = 163,
-- fcPlcupcUsp = 164, lcbPlcupcUsp = 165, fcSttbGlsyStyle = 166, lcbSttbGlsyStyle = 167, fcPlgosl = 168, lcbPlgosl = 169, fcPlcocx = 170, lcbPlcocx = 171, fcPlcfBteLvc = 172, lcbPlcfBteLvc = 173,
-- dwLowDateTime = 174, dwHighDateTime = 175, fcPlcfLvcPre10 = 176, lcbPlcfLvcPre10 = 177, fcPlcfAsumy = 178, lcbPlcfAsumy = 179, fcPlcfGram = 180, lcbPlcfGram = 181, fcSttbListNames = 182,
-- lcbSttbListNames = 183, fcSttbfUssr = 184, lcbSttbfUssr = 185

type FileInfoBlockRgFcLcb97 = Array Int Word32


data FileInfoBlockRgFcLcbBlob = FileInfoBlockRgFcLcbBlob { rgFcLcb97        :: FileInfoBlockRgFcLcb97,
                                                           rgFcLcbBlobBytes :: B.ByteString
                                                         }
  deriving (Show)

-- FibRgCswNew
data FileInfoBlockRgCswNew = FileInfoBlockRgCswNew { rgCswNewBytes          :: B.ByteString
                                                   }
  deriving (Show)

-- File Information Block or Fib
data FileInfoBlock = FileInfoBlock { fibBase                :: FileInfoBlockBase, -- 32 bytes
                                     fibRgW                 :: FileInfoBlockRgW, -- 28 bytes
                                     fibRgLw                :: FileInfoBlockRgLw, -- 88 bytes
                                     fibRgFcLcbBlob         :: FileInfoBlockRgFcLcbBlob, -- variable bytes
                                     fibRgCswNew            :: FileInfoBlockRgCswNew --variable bytes
                                   }
  deriving (Show)

data WordDocument = WordDocument { fileInfoBlock            :: FileInfoBlock,
                                   wordRemainingBytes       :: B.ByteString
                                 }
  deriving (Show)

-- end of data types ------------------------------------------------------------------
-- instances --------------------------------------------------------------------------
instance Binary FileInfoBlock where
  put = undefined
  get = do fibBase <- get
           BinaryGet.getLazyByteString 2 -- unused csw
           fibRgW <- get
           BinaryGet.getLazyByteString 2 -- unused cslw
           fibRgLw <- get
           BinaryGet.getLazyByteString 2 -- unused cbRgFcLcb
           rgFcLcb97 <- sequence (replicate 186 BinaryGet.getWord32le)
           let remainingLcbBlobBytes = 8 * convertnFibLcb (nFib fibBase) - 744 -- 744 is a size in bytes of rgFcLcb97
           rgFcLcbBlobBytes <- BinaryGet.getLazyByteString remainingLcbBlobBytes -- just get the bytes
           BinaryGet.getLazyByteString 2 -- unused cswNew
           rgCswNewBytes <- BinaryGet.getLazyByteString $ 2 * convertnFibCsw (nFib fibBase) -- just get the bytes
           return FileInfoBlock { fibBase=fibBase,
                                  fibRgW=fibRgW,
                                  fibRgLw=fibRgLw,
                                  fibRgFcLcbBlob=FileInfoBlockRgFcLcbBlob { rgFcLcb97=listArray (0,185) rgFcLcb97,
                                                                            rgFcLcbBlobBytes=rgFcLcbBlobBytes },
                                  fibRgCswNew=FileInfoBlockRgCswNew {rgCswNewBytes=rgCswNewBytes}
                                }

instance Binary FileInfoBlockBase where
  put = undefined
  get = do wIdent <- BinaryGet.getWord16le
           nFib <- BinaryGet.getWord16le
           BinaryGet.getLazyByteString 2 -- unused
           lid <- BinaryGet.getWord16le
           pnNext <- BinaryGet.getWord16le
           fibBaseFlags1 <- BinaryGet.getWord16le
           BinaryGet.getLazyByteString 2 -- unused nFibBack
           lKey <- BinaryGet.getWord32le
           fibBaseFlags2 <- BinaryGet.getWord16le
           BinaryGet.getLazyByteString 12 -- unused reserved
           return FileInfoBlockBase { wIdent=wIdent,
                                      nFib=nFib,
                                      lid=lid,
                                      pnNext=pnNext,
                                      fibBaseFlags1=fibBaseFlags1,
                                      lKey=lKey,
                                      fibBaseFlags2=fibBaseFlags2
                                    }

instance Binary FileInfoBlockRgW where
  put = undefined
  get = do BinaryGet.getLazyByteString 26 -- unused reserved
           lidFE <- BinaryGet.getWord16le
           return FileInfoBlockRgW { lidFE=lidFE }

instance Binary FileInfoBlockRgLw where
  put = undefined
  get = do cbMac <- BinaryGet.getWord32le
           BinaryGet.getLazyByteString 8 -- unused reserved
           ccpText <- BinaryGet.getWord32le
           ccpFtn <- BinaryGet.getWord32le
           ccpHdd <- BinaryGet.getWord32le
           BinaryGet.getLazyByteString 4 -- unused reserved
           ccpAtn <- BinaryGet.getWord32le
           ccpEdn <- BinaryGet.getWord32le
           ccpTxbx <- BinaryGet.getWord32le
           ccpHdrTxBx <- BinaryGet.getWord32le
           BinaryGet.getLazyByteString 44 -- unused reserved
           return FileInfoBlockRgLw { cbMac=cbMac,
                                      ccpText=ccpText,
                                      ccpFtn=ccpFtn,
                                      ccpHdd=ccpHdd,
                                      ccpAtn=ccpAtn,
                                      ccpEdn=ccpEdn,
                                      ccpTxbx=ccpTxbx,
                                      ccpHdrTxBx=ccpHdrTxBx
                                    }

instance Binary WordDocument where
  put = undefined
  get = do fib <- get
           bytes <- BinaryGet.getRemainingLazyByteString
           return WordDocument { fileInfoBlock = fib,
                                 wordRemainingBytes=bytes
                               }
-- end of instances--------------------------------------------------------------------
-- functions --------------------------------------------------------------------------

parseDOCStream :: B.ByteString -> WordDocument
parseDOCStream = decode


getDocumentText :: WordDocument -> Int -> Word32
getDocumentText doc cp = clxOffset
    where fib = fileInfoBlock doc
          fcLcb97 = rgFcLcb97 (fibRgFcLcbBlob fib)
          clxOffset = fcClx fcLcb97

convertnFibLcb 0x00C1 = 0x005D
convertnFibLcb 0x00D9 = 0x006C
convertnFibLcb 0x0101 = 0x0088
convertnFibLcb 0x010C = 0x00A4
convertnFibLcb 0x0112 = 0x00B7
convertnFibLcb _ = undefined
convertnFibCsw 0x00C1 = 0x0
convertnFibCsw 0x00D9 = 0x0002
convertnFibCsw 0x0101 = 0x0002
convertnFibCsw 0x010C = 0x0002
convertnFibCsw 0x0112 = 0x0005
convertnFibCsw _ = undefined


fcClx :: FileInfoBlockRgFcLcb97 -> Word32
fcClx x = x ! 66 -- 66 is for fcClx, see data definition
