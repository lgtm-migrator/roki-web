{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Foldable (fold)
import Hakyll

import Config (hakyllConfig)
import qualified Config.Blog.TechBlog as TB
import qualified Config.Blog.AnotherBlog as AB
import qualified Vendor.FontAwesome as FA

import qualified Rules.Media as Media
import qualified Rules.Vendor as Vendor
import qualified Rules.Src.Style as Style
import qualified Rules.Src.JavaScript as Js
import qualified Rules.IndexPage as IP

import qualified Rules.Blog as B

techBlogConf :: B.BlogConfig Rules
techBlogConf = B.BlogConfig {
    B.blogName = TB.blogName
  , B.blogTagBuilder = TB.buildTags
  , B.blogTagPagesPath = TB.tagPagesPath
  , B.blogEntryPattern = TB.entryPattern
  , B.blogEntryFilesPattern = TB.entryFilesPattern
  , B.blogAtomConfig = TB.atomConfig
  , B.blogContentSnapshot = TB.contentSnapshot
  , B.blogYearlyArchivesBuilder = TB.buildYearlyArchives
  , B.blogMonthlyArchivesBuilder = TB.buildMonthlyArchives
  , B.blogYearlyPagePath = TB.yearlyPagePath
  , B.blogMonthlyPagePath = TB.monthlyPagePath
  }

diaryConf :: B.BlogConfig Rules
diaryConf = B.BlogConfig {
    B.blogName = AB.blogName
  , B.blogTagBuilder = AB.buildTags
  , B.blogTagPagesPath = AB.tagPagesPath
  , B.blogEntryPattern = AB.entryPattern
  , B.blogEntryFilesPattern = AB.entryFilesPattern
  , B.blogAtomConfig = AB.atomConfig
  , B.blogContentSnapshot = AB.contentSnapshot
  , B.blogYearlyArchivesBuilder = AB.buildYearlyArchives
  , B.blogMonthlyArchivesBuilder = AB.buildMonthlyArchives
  , B.blogYearlyPagePath = AB.yearlyPagePath
  , B.blogMonthlyPagePath = AB.monthlyPagePath
  }

main :: IO ()
main = hakyllWith hakyllConfig $ do
    Media.rules  >> Vendor.rules >> Style.rules >> Js.rules
    faIcons <- fold <$> preprocess FA.loadFontAwesome

    B.blogRules techBlogConf faIcons
    B.blogRules diaryConf faIcons
    IP.rules [techBlogConf, diaryConf] faIcons

    match "contents/templates/**" $ compile templateBodyCompiler
    
