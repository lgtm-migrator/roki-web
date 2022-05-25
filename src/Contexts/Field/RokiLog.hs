module Contexts.Field.RokiLog (
    footerAdditionalComponent
) where

import           Contexts.Field.RokiLog.GAdsense
import           Contexts.Field.RokiLog.PowertedBy
import           Lucid.Base                        (Html)

footerAdditionalComponent :: Html ()
footerAdditionalComponent = gAdSenseFooter <> haskellJpLogo
