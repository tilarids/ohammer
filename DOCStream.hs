module DOCStream  where

import Data.Binary
import Data.Binary.Get as BinaryGet
import qualified Data.ByteString.Lazy as B

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
data FileInfoBlockRgFcLcb97 = FileInfoBlockRgFcLcb97 { fcStshfOrig :: Word32, lcbStshfOrig :: Word32, fcStshf :: Word32, lcbStshf :: Word32, fcPlcffndRef :: Word32, lcbPlcffndRef :: Word32,
                                                       fcPlcffndTxt :: Word32, lcbPlcffndTxt :: Word32, fcPlcfandRef :: Word32, lcbPlcfandRef :: Word32, fcPlcfandTxt :: Word32,
                                                       lcbPlcfandTxt :: Word32, fcPlcfSed :: Word32, lcbPlcfSed :: Word32, fcPlcPad :: Word32, lcbPlcPad :: Word32, fcPlcfPhe :: Word32,
                                                       lcbPlcfPhe :: Word32, fcSttbfGlsy :: Word32, lcbSttbfGlsy :: Word32, fcPlcfGlsy :: Word32, lcbPlcfGlsy :: Word32, fcPlcfHdd :: Word32,
                                                       lcbPlcfHdd :: Word32, fcPlcfBteChpx :: Word32, lcbPlcfBteChpx :: Word32, fcPlcfBtePapx :: Word32, lcbPlcfBtePapx :: Word32, fcPlcfSea :: Word32,
                                                       lcbPlcfSea :: Word32, fcSttbfFfn :: Word32, lcbSttbfFfn :: Word32, fcPlcfFldMom :: Word32, lcbPlcfFldMom :: Word32, fcPlcfFldHdr :: Word32,
                                                       lcbPlcfFldHdr :: Word32, fcPlcfFldFtn :: Word32, lcbPlcfFldFtn :: Word32, fcPlcfFldAtn :: Word32, lcbPlcfFldAtn :: Word32, fcPlcfFldMcr :: Word32,
                                                       lcbPlcfFldMcr :: Word32, fcSttbfBkmk :: Word32, lcbSttbfBkmk :: Word32, fcPlcfBkf :: Word32, lcbPlcfBkf :: Word32, fcPlcfBkl :: Word32,
                                                       lcbPlcfBkl :: Word32, fcCmds :: Word32, lcbCmds :: Word32, fcUnused1 :: Word32, lcbUnused1 :: Word32, fcSttbfMcr :: Word32, lcbSttbfMcr :: Word32,
                                                       fcPrDrvr :: Word32, lcbPrDrvr :: Word32, fcPrEnvPort :: Word32, lcbPrEnvPort :: Word32, fcPrEnvLand :: Word32, lcbPrEnvLand :: Word32,
                                                       fcWss :: Word32, lcbWss :: Word32, fcDop :: Word32, lcbDop :: Word32, fcSttbfAssoc :: Word32, lcbSttbfAssoc :: Word32, fcClx :: Word32,
                                                       lcbClx :: Word32, fcPlcfPgdFtn :: Word32, lcbPlcfPgdFtn :: Word32, fcAutosaveSource :: Word32, lcbAutosaveSource :: Word32,
                                                       fcGrpXstAtnOwners :: Word32, lcbGrpXstAtnOwners :: Word32, fcSttbfAtnBkmk :: Word32, lcbSttbfAtnBkmk :: Word32, fcUnused2 :: Word32,
                                                       lcbUnused2 :: Word32, fcUnused3 :: Word32, lcbUnused3 :: Word32, fcPlcSpaMom :: Word32, lcbPlcSpaMom :: Word32, fcPlcSpaHdr :: Word32,
                                                       lcbPlcSpaHdr :: Word32, fcPlcfAtnBkf :: Word32, lcbPlcfAtnBkf :: Word32, fcPlcfAtnBkl :: Word32, lcbPlcfAtnBkl :: Word32, fcPms :: Word32,
                                                       lcbPms :: Word32, fcFormFldSttbs :: Word32, lcbFormFldSttbs :: Word32, fcPlcfendRef :: Word32, lcbPlcfendRef :: Word32, fcPlcfendTxt :: Word32,
                                                       lcbPlcfendTxt :: Word32, fcPlcfFldEdn :: Word32, lcbPlcfFldEdn :: Word32, fcUnused4 :: Word32, lcbUnused4 :: Word32, fcDggInfo :: Word32,
                                                       lcbDggInfo :: Word32, fcSttbfRMark :: Word32, lcbSttbfRMark :: Word32, fcSttbfCaption :: Word32, lcbSttbfCaption :: Word32, fcSttbfAutoCaption :: Word32,
                                                       lcbSttbfAutoCaption :: Word32, fcPlcfWkb :: Word32, lcbPlcfWkb :: Word32, fcPlcfSpl :: Word32, lcbPlcfSpl :: Word32, fcPlcftxbxTxt :: Word32,
                                                       lcbPlcftxbxTxt :: Word32, fcPlcfFldTxbx :: Word32, lcbPlcfFldTxbx :: Word32, fcPlcfHdrtxbxTxt :: Word32, lcbPlcfHdrtxbxTxt :: Word32,
                                                       fcPlcffldHdrTxbx :: Word32, lcbPlcffldHdrTxbx :: Word32, fcStwUser :: Word32, lcbStwUser :: Word32, fcSttbTtmbd :: Word32, lcbSttbTtmbd :: Word32,
                                                       fcCookieData :: Word32, lcbCookieData :: Word32, fcPgdMotherOldOld :: Word32, lcbPgdMotherOldOld :: Word32, fcBkdMotherOldOld :: Word32,
                                                       lcbBkdMotherOldOld :: Word32, fcPgdFtnOldOld :: Word32, lcbPgdFtnOldOld :: Word32, fcBkdFtnOldOld :: Word32, lcbBkdFtnOldOld :: Word32,
                                                       fcPgdEdnOldOld :: Word32, lcbPgdEdnOldOld :: Word32, fcBkdEdnOldOld :: Word32, lcbBkdEdnOldOld :: Word32, fcSttbfIntlFld :: Word32, lcbSttbfIntlFld :: Word32,
                                                       fcRouteSlip :: Word32, lcbRouteSlip :: Word32, fcSttbSavedBy :: Word32, lcbSttbSavedBy :: Word32, fcSttbFnm :: Word32, lcbSttbFnm :: Word32,
                                                       fcPlfLst :: Word32, lcbPlfLst :: Word32, fcPlfLfo :: Word32, lcbPlfLfo :: Word32, fcPlcfTxbxBkd :: Word32, lcbPlcfTxbxBkd :: Word32,
                                                       fcPlcfTxbxHdrBkd :: Word32, lcbPlcfTxbxHdrBkd :: Word32, fcDocUndoWord9 :: Word32, lcbDocUndoWord9 :: Word32, fcRgbUse :: Word32, lcbRgbUse :: Word32,
                                                       fcUsp :: Word32, lcbUsp :: Word32, fcUskf :: Word32, lcbUskf :: Word32, fcPlcupcRgbUse :: Word32, lcbPlcupcRgbUse :: Word32, fcPlcupcUsp :: Word32,
                                                       lcbPlcupcUsp :: Word32, fcSttbGlsyStyle :: Word32, lcbSttbGlsyStyle :: Word32, fcPlgosl :: Word32, lcbPlgosl :: Word32, fcPlcocx :: Word32,
                                                       lcbPlcocx :: Word32, fcPlcfBteLvc :: Word32, lcbPlcfBteLvc :: Word32, dwLowDateTime :: Word32, dwHighDateTime :: Word32, fcPlcfLvcPre10 :: Word32,
                                                       lcbPlcfLvcPre10 :: Word32, fcPlcfAsumy :: Word32, lcbPlcfAsumy :: Word32, fcPlcfGram :: Word32, lcbPlcfGram :: Word32, fcSttbListNames :: Word32,
                                                       lcbSttbListNames :: Word32, fcSttbfUssr :: Word32, lcbSttbfUssr :: Word32
                                                     }
  deriving (Show)

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
           let remainingLcbBlobBytes = 8 * convertnFibLcb (nFib fibBase) - 744 -- 744 is a size in bytes of rgFcLcb97
           rgFcLcb97 <- get
           rgFcLcbBlobBytes <- BinaryGet.getLazyByteString remainingLcbBlobBytes -- just get the bytes
           BinaryGet.getLazyByteString 2 -- unused cswNew
           rgCswNewBytes <- BinaryGet.getLazyByteString $ 2 * convertnFibCsw (nFib fibBase) -- just get the bytes
           return FileInfoBlock { fibBase=fibBase,
                                  fibRgW=fibRgW,
                                  fibRgLw=fibRgLw,
                                  fibRgFcLcbBlob=FileInfoBlockRgFcLcbBlob { rgFcLcb97=rgFcLcb97,
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
instance Binary FileInfoBlockRgFcLcb97 where
  put = undefined
  get = undefined

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
