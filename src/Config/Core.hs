{-# LANGUAGE OverloadedStrings #-}
module Config.Core (
    contentsRoot,
    hakyllConfig,
    readerOptions,
    tagSoupOption,
    timeZoneJST,
    defaultTimeLocale'
) where

import Control.Monad (liftM2)
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Time.Format (TimeLocale (..), defaultTimeLocale)
import Data.Time.LocalTime (TimeZone (..))
import Hakyll
import System.FilePath (takeFileName)
import Text.Pandoc.Options (ReaderOptions (..), Extension (..), enableExtension, disableExtension)
import qualified Text.HTML.TagSoup as T

contentsRoot :: FilePath
contentsRoot = "contents"

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration {
    destinationDirectory = "docs"
  , storeDirectory = ".cache"
  , tmpDirectory = ".cache/tmp"
  , previewHost = "127.0.0.1"
  , previewPort = 8888
  , inMemoryCache = True
  , ignoreFile = ignoreFile'
  }
  where
    ignoreFile' = foldr1 (liftM2 (||)) 
        [isPrefixOf ".", isPrefixOf "#", isSuffixOf "~", isSuffixOf ".swp"] . takeFileName

readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions {
    readerExtensions = enableExtension Ext_east_asian_line_breaks $
        enableExtension Ext_emoji $
        disableExtension Ext_citations $
        readerExtensions defaultHakyllReaderOptions
    }

tagSoupOption :: T.RenderOptions String
tagSoupOption = T.RenderOptions {
    T.optRawTag = (`elem` ["script", "style"]) . map toLower
  , T.optMinimize = (`elem` minimize) . map toLower
  , T.optEscape = T.escapeHTML
  }
  where
    minimize = ["area", "br", "col", "embed", "hr", "img", "input", "meta", "link", "param"]

timeZoneJST :: TimeZone
timeZoneJST = TimeZone (9 * 60) False "JST"

defaultTimeLocale' :: TimeLocale
defaultTimeLocale' = defaultTimeLocale {
    knownTimeZones = knownTimeZones defaultTimeLocale <> [timeZoneJST]
    }

