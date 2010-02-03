module Main where

import PPTStream
import qualified Data.ByteString.Lazy as B

dumpPPT file = sHeaderInfo ++ sDirectoryInfo
    where sHeaderInfo = streamHeaderInfo pptStream
          sDirectoryInfo = streamDirectoryInfo pptStream
          pptStream = parsePPTStream file

main = do
    input  <- B.readFile "test.ppt"
    -- writeFile "dump.out" (dumpPPT input)
    B.writeFile "PowerPoint Document.dump" (extractEntry input "PowerPoint Document")

