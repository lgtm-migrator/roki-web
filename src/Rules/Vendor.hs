module Rules.Vendor (
    rules
) where

import Control.Monad (zipWithM_)
import Hakyll
import Config.RegexUtils (intercalateDir)

vendRule :: Pattern -> FilePath -> Rules ()
vendRule inpat outpath = match inpat $ do
    route $ constRoute $ outpath
    compile compressCssCompiler

rules :: Rules ()
rules = zipWithM_ vendRule
        [fontAwesomeSVGPath, bulmaPath, bulmaToolTipPath]
        [intercalateDir ["vendor", "fontawesome", "style.css"]
       , intercalateDir ["vendor", "bulma", "bulma.min.css"]
       , intercalateDir ["vendor", "bulma", "bulma-tooltip.min.css"]
       ]
    where
        fontAwesomeSVGPath = fromGlob $ intercalateDir 
            ["node_modules", "@fortawesome", "fontawesome-svg-core", "styles.css"]
        bulmaPath = fromGlob $ intercalateDir
            ["node_modules", "bulma", "css", "bulma.min.css"]
        bulmaToolTipPath = fromGlob $ intercalateDir
            ["node_modules", "@creativebulma", "bulma-tooltip", "dist", "bulma-tooltip.min.css"]

