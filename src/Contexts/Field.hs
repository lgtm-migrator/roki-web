module Contexts.Field (
    localDateField
) where

import Data.Time.Format (TimeLocale (..), formatTime)
import Data.Time.LocalTime (TimeZone (..), utcToLocalTime)
import Hakyll

localDateField :: TimeLocale -> TimeZone -> String -> String -> Context a
localDateField locale zone key format = field key $
    fmap (formatTime locale format . utcToLocalTime zone) . getItemUTC locale . itemIdentifier
