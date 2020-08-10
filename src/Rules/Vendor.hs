module Rules.Vendor (
    rules
) where

import Control.Monad (zipWithM_)
import Hakyll
import Config.RegexUtils (intercalateDir)

vendRule :: Pattern -> FilePath -> Rules ()
vendRule inpat outpath = match inpat $ do
    route $ constRoute outpath
    compile compressCssCompiler

rules :: Bool -> Rules ()
rules isPreview = do
    zipWithM_ vendRule
       [ fontAwesomeSVGPath
 --      , bulmaPath
       , bulmaToolTipPath
       , highlightPath
       , katexCssPath
       ]
       [ intercalateDir ["vendor", "fontawesome", "style.css"]
 --      , intercalateDir ["vendor", "bulma", "bulma.min.css"]
       , intercalateDir ["vendor", "bulma", "bulma-tooltip.min.css"]
       , intercalateDir ["vendor", "highlight", "highlight.css"]
       , intercalateDir ["vendor", "katex", "katex.min.css"]
       ] 

    match (fromGlob $ intercalateDir ["node_modules", "katex", "dist", "fonts", "**"]) $ do
        route $ gsubRoute "node_modules/katex/dist/" (const "vendor/katex/")
        compile copyFileCompiler

    if not isPreview then return () else do
        match (fromGlob $ intercalateDir ["node_modules", "katex", "dist", "katex.min.js"]) $ do
            route $ gsubRoute "node_modules/katex/dist/" (const "vendor/katex/")
            compile copyFileCompiler

        match (fromGlob $ intercalateDir ["node_modules", "katex", "dist", "contrib", "auto-render.min.js"]) $ do
            route $ gsubRoute "node_modules/katex/dist/contrib/" (const "vendor/katex/")
            compile copyFileCompiler
    where
        fontAwesomeSVGPath = fromGlob $ intercalateDir 
            ["node_modules", "@fortawesome", "fontawesome-svg-core", "styles.css"]
 --       bulmaPath = fromGlob $ intercalateDir
 --           ["node_modules", "bulma", "css", "bulma.min.css"]
        bulmaToolTipPath = fromGlob $ intercalateDir
            ["node_modules", "@creativebulma", "bulma-tooltip", "dist", "bulma-tooltip.min.css"]
        katexCssPath = fromGlob $ intercalateDir
            ["node_modules", "katex", "dist", "katex.min.css"]
        highlightPath = fromGlob $ intercalateDir
            ["external", "hakyll-css", "css", "tango.css"]

