module PPTStream where

import OLEStorage

parsePPTStream = parseDocument

streamHeaderInfo :: OLEDocument -> String
streamHeaderInfo (OLEDocument header _) = show header

streamDirectoryInfo :: OLEDocument -> String
streamDirectoryInfo _ = ""

