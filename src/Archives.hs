{-# LANGUAGE OverloadedStrings #-}

module Archives ( 
    Archives (..)
  , YearlyArchives
  , MonthlyArchives
  , archivesRules
  , buildYearlyArchives
  , buildMonthlyArchives
 -- , yearMonthArchiveField
) where

import Control.Monad
-- import Control.Monad.Trans (lift)
-- import Data.Function (on)
-- import Data.List (sortBy)
import qualified Data.Map as M
-- import Data.Maybe
import qualified Data.Set as S
-- import qualified Data.Text as T
-- import qualified Data.Text.Lazy as TL
import Data.Time.Format
import Data.Time.LocalTime
import Hakyll
-- import Lucid.Base
-- import Lucid.Html5

data Archives k = Archives { 
    archivesMap :: [(k, [Identifier])]
   , archivesMakeId :: k -> Identifier
   , archivesDependency :: Dependency
   }

type YearlyArchives = Archives String
type MonthlyArchives = Archives (String, String)

buildArchivesWith :: (MonadMetadata m, Ord k) => (Identifier -> m [k]) -> Pattern -> (k -> Identifier) -> m (Archives k)
buildArchivesWith f pattern makeId = do
    ids <- getMatches pattern
    am  <- M.toList <$> foldM addToMap M.empty ids
    return $ Archives am makeId $ PatternDependency pattern (S.fromList ids)
    where 
        addToMap m i = do
            ks <- f i
            let m' = M.fromList $ zip ks $ repeat [i]
            return $ M.unionWith (++) m m'

archivesRules :: Archives a -> (a -> Pattern -> Rules ()) -> Rules ()
archivesRules archives rules = forM_ (archivesMap archives) $ \(key, identifiers) ->
    rulesExtraDependencies [archivesDependency archives] $
        create [archivesMakeId archives key] $
            rules key $ fromList identifiers

buildYearlyArchives :: (MonadMetadata m, MonadFail m) => TimeLocale -> TimeZone -> Pattern -> (String -> Identifier) -> m YearlyArchives
buildYearlyArchives locale zone = buildArchivesWith $ \i ->
    return . formatTime locale "%Y" . utcToLocalTime zone <$> getItemUTC locale i

buildMonthlyArchives :: (MonadMetadata m, MonadFail m) => TimeLocale -> TimeZone -> Pattern -> ((String, String) -> Identifier) -> m MonthlyArchives
buildMonthlyArchives locale zone = buildArchivesWith $ \i -> do
    time <- utcToLocalTime zone <$> getItemUTC locale i
    let y = formatTime locale "%Y" time
        m = formatTime locale "%m" time
    return [(y, m)]

{-
yearMonthArchiveField :: String -> YearlyArchives -> MonthlyArchives -> Maybe String -> Context a
yearMonthArchiveField key ya ma pageYear = field key $ const $ 
    buildYearMonthArchiveField ya ma pageYear

buildYearMonthArchiveField :: YearlyArchives -> MonthlyArchives -> Maybe String -> Compiler String
buildYearMonthArchiveField ya ma pageYear = fmap TL.unpack $ renderTextT $
    ul_ [class_ "archive-tree"] $ do
        let yearMap = sortBy (flip compare `on` (read :: String -> Int) . fst) $ archivesMap ya
            getUrl = lift . fmap (toUrl . fromMaybe "#") . getRoute

        forM_ yearMap $ \(year, yids) ->
            li_ $ do
                let monthMap = sortBy (flip compare `on` (read :: String -> Int) . snd . fst) $
                        filter ((== year) . fst . fst) $ archivesMap ma
                    treeLael = T.pack $ "tree-label-" ++ year

                input_ $ [class_ "tree-toggle", type_ "checkbox", id_ treeLael] ++
                    [checked_ | maybe False (== year) pageYear]
                label_ [class_ "tree-toggle-button", for_ treeLael] $ do
                    i_ [classes_ ["fas", "fa-angle-right", "fa-fw"]] ""
                    i_ [classes_ ["fas", "fa-angle-down", "fa-fw"]] ""

                yurl <- getUrl $ archivesMakeId ya year
                a_ [href_ (T.pack yurl)] $
                    toHtml $ year ++ " (" ++ show (length yids) ++ ")"

                ul_ [class_ "tree-child"] $
                    forM_ monthMap $ \(mk@(_, month), mids) ->
                        li_ $ do
                            murl <- getUrl $ archivesMakeId ma mk
                            a_ [href_ (T.pack murl)] $
                                toHtml $ year ++ "/" ++ month ++  " (" ++ show (length mids) ++ ")"
                                -}
