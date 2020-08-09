{-# LANGUAGE OverloadedStrings #-}
module Contexts.Core (
    blogTitleCtx,
    siteCtx,
    postCtx,
    listCtx,
    katexJsCtx
) where

import qualified Data.Text.Lazy as TL
import Lucid.Base (renderText)
import Lucid.Html5
import Hakyll

import Config (timeZoneJST, defaultTimeLocale', siteName)
import qualified Config.Blog.TechBlog as TB
import qualified Config.Blog.AnotherBlog as BA
import Contexts.Field (localDateField, tagsField', descriptionField, imageField)

dateCtx :: Context String
dateCtx = localDateField defaultTimeLocale' timeZoneJST "date" "%Y/%m/%d %R"

blogTitleCtx :: String -> Context String
blogTitleCtx = constField "blog-title"

techBlogCtx :: Context String
techBlogCtx = constField "tech-blog-title" TB.blogName
    <> constField "tech-blog-issue-req" "https://github.com/falgon/roki-web/issues/new/choose"

privBlogCtx :: Context String
privBlogCtx = constField "diary-title" BA.blogName

blogCtx :: Context String
blogCtx = techBlogCtx <> privBlogCtx

authorCtx :: Context String
authorCtx = constField "author-name" "Roki"
    <> constField "author-avator" "/images/avator/prof1000x1000.png"
    <> constField "author-sex" "Male"
    <> constField "author-locale" "Tokyo, JP"
    <> constField "author-fav" "Beer, Coffee"
    <> constField "author-interested" 
        "・FP&#10;・Compiler&#10;・Category theory&#10;・Low layer networking, Infrastructure"
    <> constField "author-job" "Engineer"
    <> constField "author-github" "falgon"
    <> constField "author-twitter" "530506"
    <> constField "author-tumblr" "0x35"
    <> constField "author-stackoverflow" "8345717"
    <> constField "author-steam" "r0k1"
    <> constField "author-yukicoder" "3223"
    <> constField "author-teratail" "kjfkhfhgx"

siteCtx :: Context String
siteCtx = constField "lang" "ja"
    <> constField "site-title" siteName
    <> constField "site-description" "This is a Roki's website."
    <> constField "copyright" "copyright &copy; 2016~ Roki All Rights Reserved."
    <> blogCtx
    <> authorCtx

postCtx :: Bool -> Tags -> Context String
postCtx isPreview tags = dateCtx
    <> tagsField' "tags" tags
    <> descriptionField "description" 150
    <> imageField "image"
    <> siteCtx
    <> defaultContext
    <> if isPreview then katexJsCtx else mempty

listCtx :: Bool -> Context String
listCtx isPreview = siteCtx
    <> bodyField "body"
    <> metadataField
    <> urlField "url"
    <> pathField "path"
    <> if isPreview then katexJsCtx else mempty

katexJsCtx :: Context String
katexJsCtx = constField "katex-script" $ TL.unpack $ renderText $ do
    script_ [defer_ "", type_ "text/javascript", src_ "/vendor/katex/katex.min.js"] TL.empty
    script_ [defer_ "", type_ "text/javascript", src_ "/vendor/katex/auto-render.min.js"] TL.empty
