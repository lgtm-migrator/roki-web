{-# LANGUAGE OverloadedStrings #-}
module Rules.Blog.Search (
    searchBoxResult
) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Lucid.Base (renderText)
import Lucid.Html5

searchBoxResult :: String -> String
searchBoxResult cx = TL.unpack $ renderText $ do
    script_ [async_ "", src_ $ "https://cse.google.com/cse.js?cx=" <> T.pack cx] T.empty
    div_ [class_ "gcse-searchresults-only"] ""
