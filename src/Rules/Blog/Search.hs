{-# LANGUAGE OverloadedStrings #-}
module Rules.Blog.Search (
    searchBoxResult
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Lucid.Base (renderText)
import Lucid.Html5

searchBoxResult :: String -> String
searchBoxResult cx = TL.unpack $ renderText $ div_ [class_ "gcse-searchresults-only"] ""
