{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Foldable (fold)
import Hakyll
import Hakyll.Web.Sass
import qualified FontAwesome as FA
import Media

hakyllConfig :: Configuration
hakyllConfig = defaultConfiguration {
    destinationDirectory = "doc"
  , storeDirectory = ".cache"
  , tmpDirectory = ".cache/tmp"
  , previewHost = "127.0.0.1"
  , previewPort = 8888
  }

main :: IO ()
main = hakyllWith hakyllConfig $ do
    faIcons <- fold <$> preprocess FA.loadFontAwesome
    
    match "contents/images/**/*.svg" $ do
        route $ gsubRoute "contents/" $ const ""
        compile $ optimizeSVGCompiler ["-p", "4"]

    match "contents/images/*" $ do
        route $ gsubRoute "contents/" $ const ""
        compile copyFileCompiler

    match "contents/css/*" $ do
        route $ gsubRoute "contents/css/" $ const "style/"
        compile compressCssCompiler

    scssDepend <- makePatternDependency "contents/scss/*/**.scss"
    match "contents/scss/*/**.scss" $ compile getResourceBody
    rulesExtraDependencies [scssDepend] $
        match "contents/scss/*.scss" $ do
            route $ gsubRoute "contents/scss/" $ const "style/"
            compile $ fmap compressCss <$> sassCompiler

    match "node_modules/@fortawesome/fontawesome-svg-core/styles.css" $ do
        route $ constRoute "vendor/fontawesome/style.css"
        compile compressCssCompiler

    match "node_modules/bulma/css/bulma.min.css" $ do
        route $ constRoute "vendor/bulma/bulma.min.css"
        compile compressCssCompiler

    match "node_modules/@creativebulma/bulma-tooltip/dist/bulma-tooltip.min.css" $ do
        route $ constRoute "vendor/bulma/bulma-tooltip.min.css"
        compile compressCssCompiler

    match (fromList ["contents/pages/about.rst", "contents/pages/contact.markdown"]) $ do
        route $ gsubRoute "contents/pages/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "contents/templates/default.html" defaultContext
            >>= relativizeUrls

    match "contents/posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "contents/templates/post.html"    postCtx
            >>= loadAndApplyTemplate "contents/templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "contents/posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "contents/templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "contents/templates/default.html" archiveCtx
                >>= relativizeUrls

    match "contents/pages/index.html" $ do
        route $ constRoute "index.html"
        compile $ do
            posts <- recentFirst =<< loadAll "contents/posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "contents/templates/default.html" indexCtx
                >>= relativizeUrls
                >>= FA.render faIcons

    match "contents/templates/*" $ compile templateBodyCompiler


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
