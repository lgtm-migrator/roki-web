{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Foldable (fold)
import Hakyll
import System.FilePath ((</>))

import Archives
import Config
import qualified Config.RokiLog as CRL
import Contexts (postCtx, siteCtx, listCtx)
import Contexts.Field (tagCloudField')
import Utils (absolutizeUrls, makePageIdentifier, modifyExternalLinkAttr)
import qualified FontAwesome as FA

import qualified Rules.Media as Media
import qualified Rules.Vendor as Vendor
import qualified Rules.Src.Style as Style
import qualified Rules.Src.JavaScript as Js
import qualified Rules.IndexPage as IP

listPageRules :: Maybe String -> FA.FontAwesomeIcons -> Tags -> Snapshot -> Paginate -> Rules ()
listPageRules title faIcons tags snp pgs = paginateRules pgs $ \pn pat -> do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAllSnapshots pat snp
        let blogCtx = listField "posts" postCtx' (return posts)
                <> paginateContext pgs pn
                <> maybe missingField (constField "title") title
                <> listCtx
                <> tagCloudField' "tag-cloud" tags
            postCtx' = teaserField "teaser" snp <> postCtx tags

        makeItem ""
            >>= loadAndApplyTemplate "contents/templates/blog/content.html" blogCtx
            >>= loadAndApplyTemplate "contents/templates/blog/default.html" blogCtx
            >>= modifyExternalLinkAttr
            >>= FA.render faIcons


rokiLogRules :: FA.FontAwesomeIcons -> Rules Tags
rokiLogRules faIcons = do
    tags <- CRL.buildTags 
    let postCtx' = postCtx tags <> tagCloudField' "tag-cloud" tags

    -- each posts
    match CRL.entryPattern $ do
        route $ gsubRoute "contents/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompilerWith readerOptions defaultHakyllWriterOptions
            >>= absolutizeUrls
            >>= saveSnapshot CRL.contentSnapshot
            >>= loadAndApplyTemplate "contents/templates/post.html" postCtx'
            >>= loadAndApplyTemplate "contents/templates/blog/default.html" postCtx'
            >>= modifyExternalLinkAttr
            >>= FA.render faIcons

    match CRL.entryFilesPattern $ do
        route $ gsubRoute "contents/" (const "")
        compile copyFileCompiler

    -- tag rules
    tagsRules tags $ \tag pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier $ CRL.tagPagesPath tag
            title = "Tagged posts: " <> tag
        in buildPaginateWith grouper pat makeId 
            >>= listPageRules (Just title) faIcons tags CRL.contentSnapshot

    -- yearly paginate
    yearlyArchives <- CRL.buildYearlyArchives
    archivesRules yearlyArchives $ \year pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier $ CRL.yearlyPagePath year
            title = "Yearly posts: " <> year
        in buildPaginateWith grouper pat makeId 
            >>= listPageRules (Just title) faIcons tags CRL.contentSnapshot 

    -- monthly paginate
    monthlyArchives <- CRL.buildMonthlyArchives
    archivesRules monthlyArchives $ \key@(year, month) pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst 
            makeId = makePageIdentifier $ CRL.monthlyPagePath key
            title = "Monthly posts: " <> year </> month
        in buildPaginateWith grouper pat makeId
            >>= listPageRules (Just title) faIcons tags CRL.contentSnapshot

    -- all tags
    let allTagsPagePath = "roki.log" </> "tags" </> "index.html"
    listPageRules (Just "tags") faIcons tags CRL.contentSnapshot =<<
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier allTagsPagePath
        in buildPaginateWith grouper CRL.entryPattern makeId

    -- the index page of roki.log 
    create ["roki.log/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllSnapshots CRL.entryPattern CRL.contentSnapshot
            let blogCtx = listField "posts" (postCtx tags) (return posts)
                    <> constField "title" "roki.log"
                    <> tagCloudField' "tag-cloud" tags
                    <> defaultContext
                    <> siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "contents/templates/blog/content.html" blogCtx
                >>= loadAndApplyTemplate "contents/templates/blog/default.html" blogCtx
                >>= modifyExternalLinkAttr
                >>= FA.render faIcons

    return tags

main :: IO ()
main = hakyllWith hakyllConfig $ do
    Media.rules >> Vendor.rules >> Style.rules >> Js.rules
    
    faIcons <- fold <$> preprocess FA.loadFontAwesome
    
    tags <- rokiLogRules faIcons
    IP.rules faIcons tags 
    
    match "contents/templates/**" $ compile templateBodyCompiler

