{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}
module Text.FilePath.Subst.Impl where

import           "mtl" Control.Monad.Except
import qualified "regex" Text.RE.TDFA.String as R
import qualified "regex" Text.RE.Types.Capture as R
import qualified "regex" Text.RE.Types.CaptureID as R
import qualified "regex" Text.RE.Types.Match as R
import qualified "regex" Text.RE.Types.REOptions as R
import qualified "regex" Text.RE.Types.Replace as R
import           "base" Control.Applicative
import           "base" Control.Monad
import qualified "text" Data.Text as Text
import           "text" Data.Text(Text)
import           "attoparsec" Data.Attoparsec.Text

-------------------------------------------
-- API

data Subst = Subst R.RE deriving ()

compile :: (MonadError String m)=> String -> m Subst
compile = fmap Subst
        . either throwError (R.compileRegex . sourceRegex)
        . parseOnly sourcePattern . Text.pack

quoteSource :: String -> String
quoteSource ('[':s) = "[[" ++ quoteSource s
quoteSource (']':s) = "]]" ++ quoteSource s
quoteSource (c:s) = c:quoteSource s
quoteSource ([]) = []

quoteTarget :: String -> String
quoteTarget = quoteSource

-- | Perform a one-off substitution.
--
--   'Maybe' communicates successful matches.  The polymorphic 'MonadError'
--   parameter communicates user error (invalid subst/template).
subst :: (MonadError String m)
      => String  -- ^ match patten
      -> String  -- ^ replacement template
      -> String  -- ^ string to match against
      -> m (Maybe String)
subst subst target s = do
    subst <- compile subst
    fmt <- substIntoFunc subst target
    pure $ fmt s

-- | Get a function that substitutes strings into a fixed template.
--
--   'Maybe' communicates successful matches.  The polymorphic 'MonadError'
--   parameter communicates user error (invalid template).
substIntoFunc :: (MonadError String m)
              => Subst   -- ^ match patten
              -> String  -- ^ replacement template
              -> m (String -> Maybe String) -- ^ string to match against
substIntoFunc (Subst re) target = do
    repl <- R.replace <$> targetStringToRepl target
    pure $ \s -> repl <$> s R.=~~ re

-- | Get a function that performs substitutions on templates against a fixed
--   input string. The output function operates on replacement templates.
--
--   'Maybe' communicates successful matches.  The polymorphic MonadError
--   parameter communicates user error (invalid template).
substAgainstFunc :: (MonadError String m)
                 => Subst   -- ^ match patten
                 -> String  -- ^ string to match against
                 -> Maybe (String -> m String)
substAgainstFunc (Subst re) s = maybeMatch >> Just doSubst
  where
    maybeMatch = s R.=~~ re :: Maybe ()
    doSubst target = do
        repl <- R.replace <$> targetStringToRepl target
        pure . repl $ s R.=~ re

-------------------------------------------
-- Internal Datatypes

data Capture = CaptureNamed Text
             | CapturePositional
             deriving (Eq, Show)

data Glob = GlobSingle
          | GlobDouble
          deriving (Eq, Show)

data SourceComponent = SourceCapture Capture Glob
                     | SourceLiteral Text
                     deriving (Eq, Show)


data TargetComponent = TargetCapture Capture
                     | TargetLiteral Text
                     deriving (Eq, Show)

data SourcePattern = SourcePattern [SourceComponent]
                   deriving (Eq, Show)

data TargetPattern = TargetPattern [TargetComponent]
                   deriving (Eq, Show)

-------------------------------------------

globRegex :: Glob -> String
globRegex GlobSingle = "[^/]*"
globRegex GlobDouble = ".*"

brackets :: String -> String -> String -> String
brackets pre post s = pre ++ s ++ post

sourceRegex :: SourcePattern -> String
sourceRegex (SourcePattern xs) = brackets "^" "$" (xs >>= f) where
    f (SourceCapture (CaptureNamed name) glob) =
        brackets "${" "}" (Text.unpack name)
        ++ brackets "(" ")" (globRegex glob)
    f (SourceCapture CapturePositional glob) =
        brackets "$(" ")" (globRegex glob)
    f (SourceLiteral s) = R.escapeREString (Text.unpack s)

targetRep :: TargetPattern -> String
targetRep (TargetPattern xs) = f 1 xs where
    f n (TargetCapture (CaptureNamed name):xs) = "${" ++ Text.unpack name ++ "}" ++ f n xs
    f n (TargetCapture CapturePositional:xs) = "${" ++ show n ++ "}" ++ f (succ n) xs
    f n (TargetLiteral s:xs) = simpleReplEscape (Text.unpack s) ++ f n xs
    f n [] = []

targetStringToRepl :: (MonadError String m)=> String -> m String
targetStringToRepl = fmap targetRep
                   . either throwError pure
                   . parseOnly targetPattern
                   . Text.pack

unwrapResult :: Result a -> a
unwrapResult = either error id . eitherResult

simpleReplEscape :: String -> String
simpleReplEscape = flip (>>=) $ \case '$' -> "$$"
                                      c   -> [c]

parseOnly' :: Parser a -> Text -> a
parseOnly' p = either error id . parseOnly p

-------------------------------------------
-- Parsing

patternBy mkPattern component = mkPattern <$> (many component <* endOfInput)
sourcePattern = patternBy SourcePattern sourceComponent
targetPattern = patternBy TargetPattern targetComponent

sourceComponent = sourceCapture <|> (SourceLiteral <$> literal)
targetComponent = targetCapture <|> (TargetLiteral <$> literal)

targetCapture = bracket (char '[') (char ']')
              $ TargetCapture <$> captureId

sourceCapture = bracket (char '[') (char ']')
              $ SourceCapture <$> captureId <*> sourceCaptureSpecs

sourceCaptureSpecs = (char ':' *> globSpec) <|> pure GlobSingle

globSpec = char '*' *> choice [ char '*' *> pure GlobDouble
                              , pure GlobSingle
                              ]

captureId = choice [ CaptureNamed . Text.pack <$> many1 letter
                   , pure CapturePositional
                   ]

literal = Text.pack <$> many1 literalChar
literalChar = choice
    [ satisfy (`notElem` ("[]" :: String))
    , string "[[" >> pure '['
    , string "]]" >> pure ']'
    ]

bracket :: Parser a -> Parser b -> Parser c -> Parser c
bracket pre post = (pre *>) . (<* post)
