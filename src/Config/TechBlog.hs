{-# LANGUAGE OverloadedStrings #-}
module Config.TechBlog (
    blogName,
    entryPattern,
    entryFilesPattern,
    contentSnapshot,
    tagPagesPath,
    buildTags,
    yearlyPagePath,
    buildYearlyArchives,
    monthlyPagePath,
    buildMonthlyArchives
) where

import qualified Hakyll as H
import System.FilePath ((</>))

import qualified Archives as A
import Config.Core (contentsRoot, timeZoneJST, defaultTimeLocale')
import Config.RegexUtils (intercalateDir, yyyy, mm, dd)
import Utils (sanitizeTagName)

{-# INLINE blogName #-}
blogName :: FilePath
blogName = "roki.log"

{-# INLINE postRoot #-}
postRoot :: FilePath
postRoot = intercalateDir [contentsRoot, blogName]

-- contents/roki.log/year/month/day/title/index.md
entryPattern :: H.Pattern
entryPattern = H.fromRegex $ 
    "(^" 
    <> intercalateDir [postRoot, yyyy, mm, dd, ".+", "index\\.md"] 
    <> "$)"

entryFilesPattern :: H.Pattern
entryFilesPattern = H.fromRegex $ 
    "(^" 
    <> intercalateDir [postRoot, yyyy, mm, dd, ".+", ".+"] 
    <> "$)"

{-# INLINE contentSnapshot #-}
contentSnapshot :: H.Snapshot
contentSnapshot = blogName <> ".content"

{-# INLINE tagPagesPath #-}
tagPagesPath :: FilePath -> FilePath
tagPagesPath tag = blogName </> "tags" </> sanitizeTagName tag </> "index.html"

buildTags :: H.MonadMetadata m => m H.Tags
buildTags = H.buildTags entryPattern $ H.fromFilePath . tagPagesPath
        
{-# INLINE yearlyPagePath #-}
yearlyPagePath :: FilePath -> FilePath
yearlyPagePath year = blogName </> year </> "index.html"

buildYearlyArchives :: (H.MonadMetadata m, MonadFail m) => m A.YearlyArchives
buildYearlyArchives = A.buildYearlyArchives defaultTimeLocale' timeZoneJST entryPattern $
    H.fromFilePath . yearlyPagePath
        
{-# INLINE monthlyPagePath #-}
monthlyPagePath :: (FilePath, FilePath) -> FilePath
monthlyPagePath (year, month) = blogName </> year </> month </> "index.html"

buildMonthlyArchives :: (H.MonadMetadata m, MonadFail m) => m A.MonthlyArchives
buildMonthlyArchives = A.buildMonthlyArchives defaultTimeLocale' timeZoneJST entryPattern $
    H.fromFilePath . monthlyPagePath


