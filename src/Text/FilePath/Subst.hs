module Text.FilePath.Subst
    ( compile
    , Subst
    , subst
    , substAgainstFunc
    , substIntoFunc
    , quote
    , quoteTemplate
    ) where

import Text.FilePath.Subst.Impl

-- | Quote a literal string for a subst match string.
quote :: String -> String
quote = quoteSource

-- | Quote a literal string for a replacement template.
quoteTemplate :: String -> String
quoteTemplate = quoteTarget
