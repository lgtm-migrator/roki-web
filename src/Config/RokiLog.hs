{-# LANGUAGE OverloadedStrings #-}
module Config.RokiLog (
    entryPattern,
    entryFilesPattern
) where

import Hakyll

import Config.Core (contentsRoot)
import Config.RegexUtils

postRoot :: FilePath
postRoot = intercalateDir [contentsRoot, "roki.log"]

-- contents/roki.log/year/month/day/title/index.md
entryPattern :: Pattern
entryPattern = fromRegex $ 
    "(^" 
    <> intercalateDir [postRoot, yyyy, mm, dd, ".+", "index\\.md"] 
    <> "$)"

entryFilesPattern :: Pattern
entryFilesPattern = fromRegex $ 
    "(^" 
    <> intercalateDir [postRoot, yyyy, mm, dd, ".+", ".+"] 
    <> "$)"
