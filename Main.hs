module Main where

import PPTStream
import XMLPickle
import Text.XML.HXT.Arrow
import qualified Data.ByteString.Lazy as B

-- dumpPPT file = sHeaderInfo ++ sDirectoryInfo
--     where sHeaderInfo = streamHeaderInfo pptStream
--           sDirectoryInfo = streamDirectoryInfo pptStream
--           pptStream = parsePPTStream file

main = do
    input  <- B.readFile "test.ppt"
    -- writeFile "dump.out" (dumpPPT input)
    let rawStream = extractEntry input "PowerPoint Document"
    runX ( constA (parsePPTStream rawStream)
           >>>
	   xpickleDocument xpPPTNode
             [ (a_indent, v_1)
             ] "dump.xml"
         )
    --B.writeFile "PowerPoint Document.dump" 

