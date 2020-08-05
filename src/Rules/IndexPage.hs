module Rules.IndexPage (rules) where

import Hakyll

import Config (contentsRoot)
import Config.RegexUtils (intercalateDir)
import Config.Contributions
import qualified Config.RokiLog as CRL
import Contexts (postCtx, siteCtx)
import Utils (absolutizeUrls, modifyExternalLinkAttr)
import qualified Vendor.FontAwesome as FA

type EntryPattern = Pattern

data OneBlogSetting = OneBlogSetting {
    bEntryPatten :: EntryPattern
  , bSnapShot :: Snapshot
  , bTags :: Tags
  , bKeyName :: String
  }

mkBlogCtx :: OneBlogSetting -> Compiler (Context String)
mkBlogCtx obs = do
    posts <- fmap (take 4) . recentFirst =<< loadAllSnapshots (bEntryPatten obs) (bSnapShot obs)
    return (listField (bKeyName obs) (postCtx $ bTags obs) (return posts) <> defaultContext <> siteCtx)

rules :: FA.FontAwesomeIcons -> Tags -> Rules ()
rules faIcons tags = do
    projs <- preprocess renderProjectsList
    conts <- preprocess renderContributionsTable
    
    match indexPath $ do
        route $ gsubRoute "contents/pages/" (const "")
        compile $ do
            aBlogCtx <- moreCtx projs conts <$> mkBlogCtx rokiLogSetting
            getResourceBody
                >>= absolutizeUrls
                >>= applyAsTemplate aBlogCtx
                >>= loadAndApplyTemplate rootTemplate aBlogCtx
                >>= modifyExternalLinkAttr
                >>= FA.render faIcons
    where
        moreCtx p c = mappend (constField "title" "roki.dev")
            . mappend (constField "projs" p)
            . mappend (constField "contable" c)
        indexPath = fromGlob $ intercalateDir [contentsRoot, "pages", "index.html"]
        rootTemplate = fromFilePath $ intercalateDir [contentsRoot, "templates", "site", "default.html"]
        rokiLogSetting = 
            OneBlogSetting 
                CRL.entryPattern
                CRL.contentSnapshot
                tags
                "posts"
