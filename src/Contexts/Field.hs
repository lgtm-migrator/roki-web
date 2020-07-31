module Contexts.Field (
    localDateField,
    tagsField',
    tagCloudField',
    descriptionField,
    imageField
) where

import Control.Monad (liftM2)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Format (TimeLocale (..), formatTime)
import Data.Time.LocalTime (TimeZone (..), utcToLocalTime)
import Data.Maybe (catMaybes)
import Data.List (isSuffixOf)
import qualified Text.HTML.TagSoup as TS
import Lucid.Base (Html, toHtml, renderText)
import Lucid.Html5
import Hakyll

toLink :: String -> String -> Html ()
toLink text path = a_ [href_ (T.pack $ toUrl path)] $ span_ $ toHtml text

localDateField :: TimeLocale -> TimeZone -> String -> String -> Context a
localDateField locale zone key format = field key $
    fmap (formatTime locale format . utcToLocalTime zone) . getItemUTC locale . itemIdentifier

imageField :: String -> Context String
imageField key = field key $ \item ->
    case extractImages $ TS.parseTags $ itemBody item of
        [] -> noResult ("Field " ++ key ++ ": " ++ show (itemIdentifier item) ++ "has no image")
        (src:_) -> return src
    where
        extractImages = map (TS.fromAttrib "src") . filter f
        f tag = 
            let src = TS.fromAttrib "src" tag 
                cond = not $ null src || isExternal src || ".svg" `isSuffixOf` src
            in TS.isTagOpenName "img" tag && cond

descriptionField :: String -> Int -> Context String
descriptionField key len = field key $ \_ ->
  take len . escapeHtml . concat . lines . itemBody <$> getResourceBody

tagsField' :: String -> Tags -> Context a
tagsField' key tags = field key $ \item -> do
    tags' <- getTags $ itemIdentifier item
    links <- catMaybes <$> mapM (liftM2 (<$>) toLink' (getRoute . tagsMakeId tags)) tags'
    if null links 
        then noResult ("Field " ++ key ++ ": tag not set (" ++ show (itemIdentifier item) ++ ")")
        else return $ TL.unpack $ renderText $ mconcat $ map li_ links
    where
        toLink' tag = fmap (toLink tag)

tagCloudField' :: String -> Tags -> Context a
tagCloudField' key tags = field key $ const $ renderTags toLink' concat tags
    where
        toLink' tag path = const $ const $ const $ TL.unpack $ renderText $ li_ $ toLink tag path
