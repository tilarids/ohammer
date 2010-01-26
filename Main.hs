module Main where

import PPTStream


dumpPPT file = sInfo ++ sHeader ++ sDirectory
    where sInfo = streamInfo pptStream
          sHeader = streamHeader pptStream
          sDirectory = streamDirectory pptStream
          pptStream = parsePPTStream file

main = do
    input <- readFile "test.ppt"
    writeFile "dump.out" (dumpPPT input)
