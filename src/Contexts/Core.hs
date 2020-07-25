module Contexts.Core (
    siteCtx,
    postCtx
) where

import Config (timeZoneJST, defaultTimeLocale')
import Contexts.Field (localDateField)
import Hakyll

dateCtx :: Context String
dateCtx = localDateField defaultTimeLocale' timeZoneJST "date" "%Y/%m/%d %R"

techBlogCtx :: Context String
techBlogCtx = constField "tech-blog-title" "roki.log"

privBlogCtx :: Context String
privBlogCtx = constField "priv-blog-title" "roki.dump"

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
    <> constField "author-stackoverflow" "8345717/roki"
    <> constField "author-steam" "r0k1"
    <> constField "author-yukicoder" "3223/Roki"
    <> constField "author-teratail" "kjfkhfhgx"

siteCtx :: Context String
siteCtx = constField "lang" "ja"
    <> constField "site-title" "Roki Web"
    <> constField "site-description" "This is a Roki's website."
    <> constField "copyright" "copyright &copy; 2016~ Roki All Rights Reserved."
    <> blogCtx
    <> authorCtx

postCtx :: Context String
postCtx = dateCtx
    <> siteCtx
    <> defaultContext
