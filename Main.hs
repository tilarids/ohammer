module Main where

import PPTStream
import XMLPickle

import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Core

import qualified Data.ByteString.Lazy as B

-- dumpPPT file = sHeaderInfo ++ sDirectoryInfo
--     where sHeaderInfo = streamHeaderInfo pptStream
--           sDirectoryInfo = streamDirectoryInfo pptStream
--           pptStream = parsePPTStream file

main = do
    input  <- B.readFile "test.ppt"
    -- writeFile "dump.out" (dumpPPT input)
    let rawStream = extractEntry input "PowerPoint Document"
    -- print (parsePPTStream rawStream)
    --let parsed = parsePPTStream rawStream
    --runX ( constA parsed
    --       >>>
    --       xpickleDocument xpPPTNode
    --         [ withIndent yes
    --         ] "dump.xml"
    --     )
    B.writeFile "PowerPoint Document.dump" rawStream 

