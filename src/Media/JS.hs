module Media.JS (
    compressJsCompiler
) where

import qualified Data.ByteString.Lazy.Char8 as C
import Data.Tuple.Extra (first, dupe)
import Text.Jasmine (minify)
import Hakyll (Compiler, Item, itemBody, itemSetBody, getResourceString)

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = uncurry itemSetBody . first minifyJS . dupe <$> getResourceString
    where
        minifyJS = C.unpack . minify . C.pack . itemBody
