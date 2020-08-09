module Rules.IndexPage (rules) where

import Control.Monad (forM)
import System.FilePath ((</>))
import Hakyll

import Config (contentsRoot, siteName)
import Config.RegexUtils (intercalateDir)
import Config.Contributions
import Contexts (siteCtx, blogTitleCtx)
import Utils (absolutizeUrls, modifyExternalLinkAttr)
import qualified Vendor.FontAwesome as FA
import Rules.Blog.BlogConfig

mkBlogCtx :: String -> BlogConfig m -> Compiler (Context String)
mkBlogCtx key obs = do
    posts <- fmap (take 4) . recentFirst =<< loadAllSnapshots (blogEntryPattern obs) (blogContentSnapshot obs)
    return $ listField key siteCtx' (return posts) 
        <> defaultContext 
        <> siteCtx
    where
        siteCtx' = siteCtx
            <> blogTitleCtx (blogName obs)
            <> defaultContext

rules :: [BlogConfig m] -> FA.FontAwesomeIcons -> Rules ()
rules bcs faIcons = do
    projs <- preprocess renderProjectsList
    conts <- preprocess renderContributionsTable

    match indexPath $ do
        route $ gsubRoute (contentsRoot </> "pages/") (const "")
        compile $ do
            blogs <- mconcat <$> forM bcs (\bc -> mkBlogCtx (blogName bc <> "-" <> "posts") bc)
            
            let aBlogCtx = constField "title" siteName
                    <> constField "projs" projs
                    <> constField "contable" conts
                    <> blogs
            
            getResourceBody
                >>= absolutizeUrls
                >>= applyAsTemplate aBlogCtx
                >>= loadAndApplyTemplate rootTemplate aBlogCtx
                >>= modifyExternalLinkAttr
                >>= FA.render faIcons
    where
        indexPath = fromGlob $ intercalateDir [contentsRoot, "pages", "index.html"]
        rootTemplate = fromFilePath $ intercalateDir [contentsRoot, "templates", "site", "default.html"]
