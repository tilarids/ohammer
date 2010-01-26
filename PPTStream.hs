module PPTStream where

import OLEStorage

parsePPTStream = parseHeader

streamHeaderInfo :: Header -> String
streamHeaderInfo = show

streamDirectoryInfo :: Header -> String
streamDirectoryInfo _ = ""

