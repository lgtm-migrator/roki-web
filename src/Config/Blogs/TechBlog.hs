{-# LANGUAGE OverloadedStrings #-}
module Config.Blogs.TechBlog (
    blogName
  , entryPattern
  , entryFilesPattern
  , atomConfig
  , contentSnapshot
  , tagPagesPath
  , buildTags
  , yearlyPagePath
  , buildYearlyArchives
  , monthlyPagePath
  , buildMonthlyArchives
) where

import qualified Hakyll as H
import System.FilePath ((</>))

import qualified Archives as A
import qualified Config.Blogs.Utils as BU
import Config.Site (siteName)

{-# INLINE blogName #-}
blogName :: FilePath
blogName = "roki.log"

-- contents/roki.log/year/month/day/title/index.md
entryPattern :: H.Pattern
entryPattern = BU.entryPattern blogName

entryFilesPattern :: H.Pattern
entryFilesPattern = BU.entryFilesPattern blogName

atomConfig :: H.FeedConfiguration
atomConfig = H.FeedConfiguration {
    H.feedTitle = blogName
  , H.feedDescription = "Roki tech blog"
  , H.feedAuthorName = "Roki"
  , H.feedAuthorEmail = "falgon53@yahoo.co.jp"
  , H.feedRoot = siteName </> blogName
  }

{-# INLINE contentSnapshot #-}
contentSnapshot :: H.Snapshot
contentSnapshot = BU.contentSnapshot blogName

tagPagesPath :: FilePath -> FilePath
tagPagesPath = BU.tagPagesPath blogName

buildTags :: H.MonadMetadata m => m H.Tags
buildTags = BU.buildTags blogName
        
{-# INLINE yearlyPagePath #-}
yearlyPagePath :: FilePath -> FilePath
yearlyPagePath = BU.yearlyPagePath blogName

buildYearlyArchives :: (H.MonadMetadata m, MonadFail m) => m A.YearlyArchives
buildYearlyArchives = BU.buildYearlyArchives blogName

{-# INLINE monthlyPagePath #-}
monthlyPagePath :: (FilePath, FilePath) -> FilePath
monthlyPagePath = BU.monthlyPagePath blogName

buildMonthlyArchives :: (H.MonadMetadata m, MonadFail m) => m A.MonthlyArchives
buildMonthlyArchives = BU.buildMonthlyArchives blogName


