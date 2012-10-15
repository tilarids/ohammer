{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import PPTStream
import OLEStorage
import DOCStream
import XMLPickle
import OLEStorage
import DataSpaces
import System.Environment (getArgs, withArgs)

import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Core

import System.Console.CmdArgs

import qualified Data.ByteString.Lazy as B

data OHammer =   OLEDump {    filenameOLE :: String,
                              dumpdirOLE :: String
                         }
               | PPTDump {
                              filenamePPT :: String,
                              outfilePPT :: String
                         }
                | CryptoDump {
                              filenameCrypto :: String,
                              dumpdirCrypto :: String
                         }
                deriving (Show, Data, Typeable)

oledump = OLEDump{dumpdirOLE = def, filenameOLE = def}
pptdump = PPTDump{filenamePPT = def, outfilePPT = def}
cryptodump = CryptoDump{filenameCrypto = def, dumpdirCrypto = def}

processMode (OLEDump fname dir)
    | (not $ null dir) && (not $ null fname)  = do
    input <- B.readFile fname
    dumpOLEStorage input dir
processMode (PPTDump fname outf)
    | (not $ null outf) && (not $ null fname)  = do
    input <- B.readFile fname
    let rawStream = extractFileEntry input "PowerPoint Document"
    let parsed = parsePPTStream rawStream
    --putStrLn $ show parsed
    runX ( constA parsed
           >>>
           xpickleDocument xpPPTNode
             [ withIndent yes
             ] outf
         )
    return ()
processMode (CryptoDump fname dir)
    | (not $ null dir) && (not $ null fname)  = do
    input <- B.readFile fname
    dumpCryptoStorage input dir


main = do
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) $ cmdArgs (modes [oledump, pptdump, cryptodump])
    processMode opts
