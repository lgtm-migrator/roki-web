module Utils (
    absolutizeUrls
) where

import Hakyll 
import System.FilePath ((</>), takeDirectory, normalise, isRelative)

absolutizeUrls :: Item String -> Compiler (Item String)
absolutizeUrls item = getUnderlying >>= fmap (maybe item (flip fmap item . withUrls . f)) . getRoute
    where
        f r u
            | not (isExternal u) && isRelative u = normalise $ "/" </> takeDirectory r </> u
            | otherwise = u
