{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Binary (Binary)
import Data.Foldable (fold)
import Data.Time.Format (formatTime, TimeLocale)
import Data.Time.LocalTime (utcToLocalTime, TimeZone)
import Data.Typeable (Typeable)
import Data.Maybe (catMaybes)
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..))
import Hakyll
import System.FilePath ((</>))

import Archives
import Config
import qualified Config.TechBlog as TechBlog
import Contexts (postCtx, siteCtx, listCtx, blogTitleCtx)
import Contexts.Field (tagCloudField', yearMonthArchiveField)
import Utils (absolutizeUrls, makePageIdentifier, modifyExternalLinkAttr)
import qualified Vendor.FontAwesome as FA
import qualified Vendor.KaTeX as KaTeX

import qualified Rules.Media as Media
import qualified Rules.Vendor as Vendor
import qualified Rules.Src.Style as Style
import qualified Rules.Src.JavaScript as Js
import qualified Rules.IndexPage as IP

appendFooter :: (Binary a, Typeable a, Semigroup a) => TimeLocale -> TimeZone -> Item a -> Compiler (Item a)
appendFooter locale zone item = do
    utc <- fmap Just (getItemUTC locale (itemIdentifier item))
        `catchError` const (return Nothing)
    let y = fmap (formatTime locale "%Y" . utcToLocalTime zone) utc
    appendFooterWith y item
    where 
        appendFooterWith y item' = do
            footer <- loadBody $ setVersion y "dy-footer.html"
            withItemBody (return . (<> footer)) item'

eachPostsSeries :: [Identifier] -> (Context String -> Rules ()) -> Rules ()
eachPostsSeries postIDs rules = do
    forM_ (zip3 postIDs nextPosts prevPosts) $ \(pID, np, pp) -> create [pID] $ do
        rules $ mconcat $ catMaybes [
            (field "previousPageUrl" . pageUrlOf) <$> pp
          , (field "previousPageTitle" . pageTitleOf) <$> pp
          , (field "previousPageDate" . pageDateOf) <$> pp
          , (field "nextPageUrl" . pageUrlOf) <$> np
          , (field "nextPageTitle" . pageTitleOf) <$> np
          , (field "nextPageDate" . pageDateOf) <$> np
          ]
    where
        nextPosts = tail $ map Just postIDs ++ [Nothing] 
        prevPosts = Nothing : map Just postIDs
        pageTitleOf i = const $ do
            t <- getMetadataField i "title" 
            case t of
                Nothing -> fail "no 'title' field"
                Just t' -> return $ if length t' > 6 then take 6 t' <> "..." else t'
        pageUrlOf i = const (getRoute i >>= maybe (fail "no route") (return . toUrl))
        pageDateOf i = const $ 
            getMetadataField i "date" 
                >>= maybe (fail "no 'date' field") (return . map (\x -> if x == '-' then '/' else x))

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
                <> blogTitleCtx TechBlog.blogName
            postCtx' = teaserField "teaser" snp 
                <> postCtx tags

        makeItem ""
            >>= loadAndApplyTemplate "contents/templates/blog/post-list.html" blogCtx
            >>= appendFooter defaultTimeLocale' timeZoneJST
            >>= loadAndApplyTemplate "contents/templates/blog/default.html" blogCtx
            >>= modifyExternalLinkAttr
            >>= FA.render faIcons


blogRules :: FA.FontAwesomeIcons -> Rules Tags
blogRules faIcons = do
    tags <- TechBlog.buildTags 
    let postCtx' = postCtx tags 
            <> tagCloudField' "tag-cloud" tags
            <> blogTitleCtx TechBlog.blogName
    
    -- each posts
    postIDs <- sortChronological =<< getMatches TechBlog.entryPattern
    eachPostsSeries postIDs $ \s -> do
        route $ gsubRoute "contents/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompilerWith readerOptions writerOptions
            >>= absolutizeUrls
            >>= KaTeX.render
            >>= saveSnapshot TechBlog.contentSnapshot
            >>= loadAndApplyTemplate "contents/templates/blog/post.html" (s <> postCtx')
            >>= appendFooter defaultTimeLocale' timeZoneJST
            >>= loadAndApplyTemplate "contents/templates/blog/default.html" postCtx'
            >>= modifyExternalLinkAttr
            >>= FA.render faIcons

    -- tag rules
    tagsRules tags $ \tag pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier $ TechBlog.tagPagesPath tag
            title = "Tagged posts: " <> tag
        in buildPaginateWith grouper pat makeId 
            >>= listPageRules (Just title) faIcons tags TechBlog.contentSnapshot

    -- yearly paginate
    yearlyArchives <- TechBlog.buildYearlyArchives
    archivesRules yearlyArchives $ \year pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier $ TechBlog.yearlyPagePath year
            title = "Yearly posts: " <> year
        in buildPaginateWith grouper pat makeId 
            >>= listPageRules (Just title) faIcons tags TechBlog.contentSnapshot 

    -- monthly paginate
    monthlyArchives <- TechBlog.buildMonthlyArchives
    archivesRules monthlyArchives $ \key@(year, month) pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst 
            makeId = makePageIdentifier $ TechBlog.monthlyPagePath key
            title = "Monthly posts: " <> year </> month
        in buildPaginateWith grouper pat makeId
            >>= listPageRules (Just title) faIcons tags TechBlog.contentSnapshot

    -- all tags
    let allTagsPagePath = TechBlog.blogName </> "tags" </> "index.html"
    listPageRules (Just "tags") faIcons tags TechBlog.contentSnapshot =<<
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier allTagsPagePath
        in buildPaginateWith grouper TechBlog.entryPattern makeId

    -- the index page of tech blog 
    listPageRules Nothing faIcons tags TechBlog.contentSnapshot =<<
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier (TechBlog.blogName </> "index.html")
        in buildPaginateWith grouper TechBlog.entryPattern makeId

    -- footer
    forM_ (Nothing:map (Just . fst) (archivesMap yearlyArchives)) $ \year -> maybe id version year $
        create ["dy-footer.html"] $
            compile $ do
                recent <- fmap (take 5) . recentFirst =<< 
                    loadAllSnapshots TechBlog.entryPattern TechBlog.contentSnapshot
                let ctx = listField "recent-posts" (postCtx tags) (return recent)
                        <> tagCloudField' "tag-cloud" tags
                        <> yearMonthArchiveField "archives" yearlyArchives monthlyArchives year
                        <> siteCtx
                makeItem "" >>= loadAndApplyTemplate "contents/templates/blog/footer.html" ctx

    return tags

main :: IO ()
main = hakyllWith hakyllConfig $ do
    Media.rules >> Vendor.rules >> Style.rules >> Js.rules
    
    faIcons <- fold <$> preprocess FA.loadFontAwesome
    
    tags <- blogRules faIcons
    IP.rules faIcons tags 
    
    match "contents/templates/**" $ compile templateBodyCompiler

