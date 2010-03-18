module Main where

import PPTStream
import OLEStorage
import DOCStream
import XMLPickle
import Text.XML.HXT.Arrow
import qualified Data.ByteString.Lazy as B

-- dumpPPT file = sHeaderInfo ++ sDirectoryInfo
--     where sHeaderInfo = streamHeaderInfo pptStream
--           sDirectoryInfo = streamDirectoryInfo pptStream
--           pptStream = parsePPTStream file

-- That's for PPT
--main = do
--    input  <- B.readFile "test.ppt"
--    -- writeFile "dump.out" (dumpPPT input)
--    let rawStream = extractEntry input "PowerPoint Document"
--    -- print (parsePPTStream rawStream)
--    let parsed = parsePPTStream rawStream
--    runX ( constA parsed
--           >>>
--           xpickleDocument xpPPTNode
--             [ (a_indent, v_1)
--             ] "dump.xml"
--         )
--    --B.writeFile "PowerPoint Document.dump"


main = do
    input  <- B.readFile "test.doc"
    let rawStream = extractEntry input "WordDocument"
    let parsed = parseDOCStream rawStream
    B.writeFile "WordDocument.dump" parsed
