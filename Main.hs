{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import PPTStream
import XMLPickle
import OLEStorage
import System.Environment (getArgs, withArgs)

import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Core

import System.Console.CmdArgs

import qualified Data.ByteString.Lazy as B

-- dumpPPT file = sHeaderInfo ++ sDirectoryInfo
--     where sHeaderInfo = streamHeaderInfo pptStream
--           sDirectoryInfo = streamDirectoryInfo pptStream
--           pptStream = parsePPTStream file

--main = do
--    input  <- B.readFile "test.ppt"
    -- writeFile "dump.out" (dumpPPT input)
    --let rawStream = extractEntry input "PowerPoint Document"
    -- print (parsePPTStream rawStream)
    --let parsed = parsePPTStream rawStream
    --runX ( constA parsed
    --       >>>
    --       xpickleDocument xpPPTNode
    --         [ withIndent yes
    --         ] "dump.xml"
    --     )
    --B.writeFile "PowerPoint Document.dump" rawStream

data OHammer =   OLEDump {    filename :: String,
                              dumpdir :: String
                         }
               | PPTDump {
                              pptfile :: String,
                              outfile :: String
                         }
                deriving (Show, Data, Typeable)

oledump = OLEDump{dumpdir = def, filename = def}
pptdump = PPTDump{pptfile = def, outfile = def}

processMode (OLEDump fname dir)
    | (not $ null dir) && (not $ null fname)  = do
    input <- B.readFile fname
    dumpOLEStorage input dir
processMode (PPTDump fname outf)
    | (not $ null outf) && (not $ null fname)  = do
    input <- B.readFile fname
    let rawStream = extractEntry input "PowerPoint Document"
    let parsed = parsePPTStream rawStream
    runX ( constA parsed
           >>>
           xpickleDocument xpPPTNode
             [ withIndent yes
             ] outf
         )
    return ()

main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgs (modes [oledump, pptdump])
    processMode opts
