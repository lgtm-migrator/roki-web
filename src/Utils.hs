module Utils (
    tagSoupOption
) where

import Data.Char (toLower)
import qualified Text.HTML.TagSoup as T

tagSoupOption :: T.RenderOptions String
tagSoupOption = T.RenderOptions {
    T.optRawTag = (`elem` ["script", "style"]) . map toLower
  , T.optMinimize = (`elem` minimize) . map toLower
  , T.optEscape = T.escapeHTML
  }
  where
    minimize = ["area", "br", "col", "embed", "hr", "img", "input", "meta", "link", "param"]
