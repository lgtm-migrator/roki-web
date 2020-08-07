{-# LANGUAGE OverloadedStrings #-}
module Rules.Blog (
    BlogConfig (..)
  , blogRules
) where

import Data.Binary (Binary)
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
import Contexts (postCtx, siteCtx, listCtx, blogTitleCtx)
import Contexts.Field (tagCloudField', yearMonthArchiveField)
import Utils (absolutizeUrls, makePageIdentifier, modifyExternalLinkAttr, prependBaseUrl, sanitizeDisqusName)
import qualified Vendor.FontAwesome as FA
import qualified Vendor.KaTeX as KaTeX

data BlogConfig m = BlogConfig {
    blogName :: String
  , blogTagBuilder :: m Tags
  , blogTagPagesPath :: FilePath -> FilePath
  , blogEntryPattern :: Pattern
  , blogEntryFilesPattern :: Pattern
  , blogAtomConfig :: FeedConfiguration
  , blogContentSnapshot :: String
  , blogYearlyArchivesBuilder :: m YearlyArchives
  , blogMonthlyArchivesBuilder :: m MonthlyArchives
  , blogYearlyPagePath :: FilePath -> FilePath
  , blogMonthlyPagePath :: (FilePath, FilePath) -> FilePath
  }

appendFooter :: (Binary a, Typeable a, Semigroup a) 
    => BlogConfig m
    -> TimeLocale 
    -> TimeZone 
    -> Item a 
    -> Compiler (Item a)
appendFooter bc locale zone item = do
    utc <- fmap Just (getItemUTC locale (itemIdentifier item))
        `catchError` const (return Nothing)
    appendFooterWith (fmap (formatTime locale "%Y" . utcToLocalTime zone) utc) item
    where 
        appendFooterWith y item' = do
            footer <- loadBody $ setVersion y $ fromFilePath (blogName bc <> "-footer.html")
            withItemBody (return . (<> footer)) item'

eachPostsSeries :: [Identifier] -> (Context String -> Rules ()) -> Rules ()
eachPostsSeries postIDs rules = 
    forM_ (zip3 postIDs nextPosts prevPosts) $ \(pID, np, pp) -> create [pID] $
        rules $ mconcat $ catMaybes [
            field "previousPageUrl" . pageUrlOf <$> pp
          , field "previousPageTitle" . pageTitleOf <$> pp
          , field "previousPageDate" . pageDateOf <$> pp
          , field "nextPageUrl" . pageUrlOf <$> np
          , field "nextPageTitle" . pageTitleOf <$> np
          , field "nextPageDate" . pageDateOf <$> np
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


listPageRules :: Maybe String
    -> FA.FontAwesomeIcons
    -> Tags
    -> BlogConfig m
    -> Paginate
    -> Rules ()
listPageRules title faIcons tags bc pgs = paginateRules pgs $ \pn pat -> do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAllSnapshots pat (blogContentSnapshot bc)
        let blogCtx = listField "posts" postCtx' (return posts)
                <> paginateContext pgs pn
                <> maybe missingField (constField "title") title
                <> listCtx
                <> tagCloudField' "tag-cloud" tags
                <> blogTitleCtx (blogName bc)
            postCtx' = teaserField "teaser" (blogContentSnapshot bc)
                <> postCtx tags
                <> blogTitleCtx (blogName bc)

        makeItem ""
            >>= loadAndApplyTemplate "contents/templates/blog/post-list.html" blogCtx
            >>= appendFooter bc defaultTimeLocale' timeZoneJST
            >>= loadAndApplyTemplate "contents/templates/blog/default.html" blogCtx
            >>= modifyExternalLinkAttr
            >>= FA.render faIcons

blogRules :: BlogConfig Rules -> FA.FontAwesomeIcons -> Rules ()
blogRules bc faIcons = do
    tags <- blogTagBuilder bc 
    let postCtx' = postCtx tags 
            <> tagCloudField' "tag-cloud" tags
            <> blogTitleCtx (blogName bc)
    
    -- each posts
    postIDs <- sortChronological =<< getMatches (blogEntryPattern bc)
    eachPostsSeries postIDs $ \s -> do
        route $ gsubRoute "contents/" (const "") `composeRoutes` setExtension "html"
        compile $ pandocCompilerWith readerOptions writerOptions
            >>= absolutizeUrls
            >>= saveSnapshot "feed-content"
            >>= KaTeX.render
            >>= saveSnapshot (blogContentSnapshot bc)
            >>= loadAndApplyTemplate "contents/templates/blog/post.html" 
                (s <> postCtx' <> constField "disqus" (sanitizeDisqusName (blogName bc)))
            >>= appendFooter bc defaultTimeLocale' timeZoneJST
            >>= loadAndApplyTemplate "contents/templates/blog/default.html" postCtx'
            >>= modifyExternalLinkAttr
            >>= FA.render faIcons

    match (blogEntryFilesPattern bc) $ do
        route $ gsubRoute "contents/" (const "")
        compile copyFileCompiler

    -- tag rules
    tagsRules tags $ \tag pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier $ blogTagPagesPath bc tag
            title = "Tagged posts: " <> tag
        in buildPaginateWith grouper pat makeId 
            >>= listPageRules (Just title) faIcons tags bc

    -- yearly paginate
    yearlyArchives <- blogYearlyArchivesBuilder bc
    archivesRules yearlyArchives $ \year pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier $ blogYearlyPagePath bc year
            title = "Yearly posts: " <> year
        in buildPaginateWith grouper pat makeId 
            >>= listPageRules (Just title) faIcons tags bc 

    -- monthly paginate
    monthlyArchives <- blogMonthlyArchivesBuilder bc
    archivesRules monthlyArchives $ \key@(year, month) pat ->
        let grouper = fmap (paginateEvery 5) . sortRecentFirst 
            makeId = makePageIdentifier $ blogMonthlyPagePath bc key
            title = "Monthly posts: " <> year </> month
        in buildPaginateWith grouper pat makeId
            >>= listPageRules (Just title) faIcons tags bc 

    -- all tags
    let allTagsPagePath = blogName bc </> "tags" </> "index.html"
    listPageRules (Just "tags") faIcons tags bc =<<
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier allTagsPagePath
        in buildPaginateWith grouper (blogEntryPattern bc) makeId

    -- the index page of tech blog 
    listPageRules Nothing faIcons tags bc =<<
        let grouper = fmap (paginateEvery 5) . sortRecentFirst
            makeId = makePageIdentifier (blogName bc </> "index.html")
        in buildPaginateWith grouper (blogEntryPattern bc) makeId

    -- footer
    let footerPath = fromFilePath (contentsRoot </> "templates" </> "blog" </> "footer.html")
    forM_ (Nothing:map (Just . fst) (archivesMap yearlyArchives)) $ \year -> maybe id version year $
        create [fromFilePath $ blogName bc <> "-footer.html"] $
            compile $ do
                recent <- fmap (take 5) . recentFirst =<< 
                    loadAllSnapshots (blogEntryPattern bc) (blogContentSnapshot bc)
                let ctx = listField "recent-posts" (postCtx tags) (return recent)
                        <> tagCloudField' "tag-cloud" tags
                        <> yearMonthArchiveField "archives" yearlyArchives monthlyArchives year
                        <> siteCtx
                makeItem "" >>= loadAndApplyTemplate footerPath ctx

    -- Atom Feed
    create [fromFilePath (blogName bc </> "feed" </> blogName bc <> ".xml")] $ do
        route idRoute
        compile $
            loadAllSnapshots (blogEntryPattern bc) "feed-content"
                >>= fmap (take 20) . recentFirst
                >>= mapM (prependBaseUrl (feedRoot (blogAtomConfig bc)))
                >>= renderAtom (blogAtomConfig bc) (bodyField "description" <> postCtx')

