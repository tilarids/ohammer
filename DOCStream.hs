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
data FileInfoBlockRgW = FileInfoBlockRgW {
                                         }

-- FibRgLw97
data FileInfoBlockRgLw = FileInfoBlockRgLw {
                                           }

-- FibRgFcLcb
data FileInfoBlockRgFcLcb = FileInfoBlockRgFcLcb {
                                                 }

-- FibRgCswNew
data FileInfoBlockRgCswNew = FileInfoBlockRgCswNew {
                                                   }

-- File Information Block or Fib
data FileInfoBlock = FileInfoBlock { fibBase                :: FileInfoBlockBase, -- 32 bytes
                                     fibRgW                 :: FileInfoBlockRgW, -- 28 bytes
                                     fibRgLw                :: FileInfoBlockRgLw, -- 88 bytes
                                     fibRgFcLcb             :: FileInfoBlockRgFcLcb, -- variable bytes
                                     fibRgCswNew            :: FileInfoBlockRgCswNew --variable bytes
                                   }
-- end of data types ------------------------------------------------------------------
-- instances --------------------------------------------------------------------------
instance Binary FileInfoBlock where
  put = undefined
  get = do fibBase <- get
           BinaryGet.getLazyByteString 2 -- unused csw
           return FileInfoBlock { fibBase=fibBase,
                                  fibRgW=FileInfoBlockRgW {},
                                  fibRgLw=FileInfoBlockRgLw {},
                                  fibRgFcLcb=FileInfoBlockRgFcLcb {},
                                  fibRgCswNew=FileInfoBlockRgCswNew {}
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

-- end of instances--------------------------------------------------------------------
-- functions --------------------------------------------------------------------------
parseDOCStream bs = bs
