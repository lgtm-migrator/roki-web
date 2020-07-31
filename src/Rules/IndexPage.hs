module Rules.IndexPage (rules) where

import Hakyll

import Config (contentsRoot)
import Config.RegexUtils (intercalateDir)
import Config.Contributions
import qualified Config.RokiLog as CRL
import Contexts (postCtx, siteCtx, rokiWebCtx)
import Utils (absolutizeUrls, modifyExternalLinkAttr)
import qualified FontAwesome as FA

type EntryPattern = Pattern

data OneBlogSetting = OneBlogSetting {
    bEntryPatten :: EntryPattern
  , bSnapShot :: Snapshot
  , bTags :: Tags
  , bKeyName :: String
  }

mkBlogCtx :: OneBlogSetting -> Compiler (Context String)
mkBlogCtx obs = do
    posts <- recentFirst =<< loadAllSnapshots (bEntryPatten obs) (bSnapShot obs)
    return (listField (bKeyName obs) (postCtx $ bTags obs) (return posts) <> defaultContext <> siteCtx)

rules :: FA.FontAwesomeIcons -> Tags -> Rules ()
rules faIcons tags = do
    projs <- preprocess renderProjectsList
    conts <- preprocess renderContributionsTable
    
    match indexPath $ do
        route $ gsubRoute "contents/pages/" (const "")
        compile $ do
            aBlogCtx <- addForBlogCtx projs conts <$> mkBlogCtx rokiLogSetting
            getResourceBody
                >>= absolutizeUrls
                >>= applyAsTemplate aBlogCtx
                >>= loadAndApplyTemplate defaultTemplate aBlogCtx
                >>= modifyExternalLinkAttr
                >>= FA.render faIcons
    where
        addForBlogCtx p c = mappend (constField "projs" p)
            . mappend (constField "contable" c)
            . mappend rokiWebCtx
        indexPath = fromGlob $ intercalateDir [contentsRoot, "pages", "index.html"]
        defaultTemplate = fromFilePath $ intercalateDir [contentsRoot, "templates", "default.html"]
        rokiLogSetting = 
            OneBlogSetting 
                CRL.entryPattern
                CRL.contentSnapshot
                tags
                "posts"
