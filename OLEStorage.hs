module OLEStorage where

data ByteOrder = LittleEndian | BigEndian | UnknownEndian

data BlockType = MSATType | SATType | SSATType | DirectoryType

data StreamLocation = SATLocation | SSATLocation

data MSAT = MSAT -- it's empty now

data Header =
    Header { docId                  :: Int, -- document id
             uId                    :: Int, -- unique id
             revision               :: Int,
             version                :: Int,
             byteOrder              :: ByteOrder,
             secSize                :: Int, -- sector size (usually 512 bytes)
             secSizeShort           :: Int, -- short sector size (usually 64 bytes)
             numSecSAT              :: Int, -- total number of sectors in SAT (equals the number of sector IDs
                                            -- stored in the MSAT).
             secIDFirstDirStrm      :: Int,
             minStreamSize          :: Int,
             secIDFirstSSAT         :: Int,
             numSecSSAT             :: Int,
             secIDFirstMSAT         :: Int,
             numSecMSAT             :: Int,
             masterSAT              :: MSAT -- master sector allocation table
           }
