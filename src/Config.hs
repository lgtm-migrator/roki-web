{-# LANGUAGE OverloadedStrings #-}
module Config (
    hakyllConfig,
    entryPattern,
    entryFilesPattern,
    readerOptions,
    tagSoupOption,
    timeZoneJST,
    defaultTimeLocale'
) where

import BasicPrelude (intercalate)
import Control.Monad (liftM2)
import Data.Char (toLower)
import Data.List (isPrefixOf, isSuffixOf)
import Data.String (IsString)
import Data.Time.Format (TimeLocale (..), defaultTimeLocale)
import Data.Time.LocalTime (TimeZone (..))
import Hakyll
import System.FilePath (takeFileName)
import Text.Pandoc.Options (ReaderOptions (..), Extension (..), enableExtension, disableExtension)
import qualified Text.HTML.TagSoup as T

-- NOTE: 
--  Hakyll uses the regex-tdfa library, which supports POSIX extended regular expressions
--  Ref. https://github.com/jaspervdj/hakyll/issues/524#issuecomment-282253949

-- Years from 1000 to 2999
{-# INLINE yyyy #-}
yyyy :: FilePath
yyyy = "[12][0-9]{3}"

{-# INLINE mm #-}
mm :: FilePath
mm = "(0?[1-9]|1[012])"

{-# INLINE dd #-}
dd :: FilePath
dd = "(0?[1-9]|[12][0-9]|3[01])"

intercalateDir :: (Monoid w, IsString w) => [w] -> w
intercalateDir = intercalate "/"

contentsRoot :: FilePath
contentsRoot = "contents"

postRoot :: FilePath
postRoot = intercalateDir [contentsRoot, "posts"]

-- contents/posts/year/month/day/title/index.md
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

