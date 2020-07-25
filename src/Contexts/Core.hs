module Contexts.Core (
    postCtx
) where

import Config (timeZoneJST, defaultTimeLocale')
import Contexts.Field (localDateField)
import Hakyll

postCtx :: Context String
postCtx = localDateField defaultTimeLocale' timeZoneJST "date" "%Y/%m/%d %R"
    <> defaultContext
