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

-- FibRgW97
data FileInfoBlockRgW = FileInfoBlockRgW { lidFE            :: Word16
                                         }

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

-- FibRgFcLcb
data FileInfoBlockRgFcLcbBlob = FileInfoBlockRgFcLcbBlob { rgFcLcbBlobBytes :: B.ByteString
                                                         }

-- FibRgCswNew
data FileInfoBlockRgCswNew = FileInfoBlockRgCswNew { rgCswNewBytes          :: B.ByteString
                                                   }

-- File Information Block or Fib
data FileInfoBlock = FileInfoBlock { fibBase                :: FileInfoBlockBase, -- 32 bytes
                                     fibRgW                 :: FileInfoBlockRgW, -- 28 bytes
                                     fibRgLw                :: FileInfoBlockRgLw, -- 88 bytes
                                     fibRgFcLcbBlob         :: FileInfoBlockRgFcLcbBlob, -- variable bytes
                                     fibRgCswNew            :: FileInfoBlockRgCswNew --variable bytes
                                   }
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
           rgFcLcbBlobBytes <- BinaryGet.getLazyByteString $ 8 * convertnFibLcb (nFib fibBase) -- just get the bytes
           BinaryGet.getLazyByteString 2 -- unused cswNew
           rgCswNewBytes <- BinaryGet.getLazyByteString $ 2 * convertnFibCsw (nFib fibBase) -- just get the bytes
           return FileInfoBlock { fibBase=fibBase,
                                  fibRgW=fibRgW,
                                  fibRgLw=fibRgLw,
                                  fibRgFcLcbBlob=FileInfoBlockRgFcLcbBlob { rgFcLcbBlobBytes=rgFcLcbBlobBytes },
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

-- end of instances--------------------------------------------------------------------
-- functions --------------------------------------------------------------------------

parseDOCStream bs = bs

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
