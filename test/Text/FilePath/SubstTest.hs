{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.FilePath.SubstTest
    ( testSuite
    ) where

import qualified "regex" Text.RE.TDFA.String as R
import           "mtl" Control.Monad.Except
-- import           "base" Debug.Trace
import           "attoparsec" Data.Attoparsec.Text
import           Text.FilePath.Subst.Impl
import           Text.FilePath.Subst(quote,quoteTemplate)
import           TestUtils

-- backtracking parsers like attoparsec offer little control over error
--  messages, so we don't explicitly test them, instead using a MonadError
--  instance that throws this information away.
-- (REMARK: Then what's the point of even having error messages?)
instance MonadError String Maybe where
    throwError = const Nothing
    catchError _ = const $ error "catchError: Maybe"

testSuite :: TestTree
testSuite =
    "Subst" ~:
    [ "Source parsing" ~: testSourceParsing
    , "Target parsing" ~: testTargetParsing
    , "Substitution"   ~: testSubst
    , testSubstIntoCaptures
    , testSubstAgainstCaptures
    , testProperties
    ]

testProperties :: TestTree
testProperties =
    "Properties" ~:
    [ "Quoted subst" ~: qc propQuotedSubst
    , "Quoted template" ~: qc propQuotedTemplate
    , "subst vs substIntoFunc" ~: qc propSubstSubstInto
    , "subst vs substAgainstFunc" ~: qc propSubstSubstAgainst
    ]

testSourceParsing = tree
  where
    tree =

        [ "single elements" ~:
            [ "literal"           ~: "literal"    @@: [lit "literal"]
            , "named"             ~: "[named]"    @@: [namedGlob     "named"]
            , "named glob"        ~: "[named:*]"  @@: [namedGlob     "named"]
            , "named globstar"    ~: "[named:**]" @@: [namedGlobStar "named"]
            , "numbered"          ~: "[]"         @@: [posGlob]
            , "numbered glob"     ~: "[:*]"       @@: [posGlob]
            , "numbered globstar" ~: "[:**]"      @@: [posGlobStar]
            ]

        , "empty strings are okey dokes" ~: "" @@: []

        , "escaping" ~: "ab]]c[[def" @@: [lit "ab]c[def"]

        , "a string with multiple components" ~:
            "so[]me-[example]e"
            @@: [lit "so", posGlob, lit "me-", namedGlob "example", lit "e"]

        , "things that shouldn't compile" ~:
            [ "bad names" ~:# fmap checkFail
                [ "[a1]"
                , "[asbf-sf"
                ]
            , "unpartnered bracket" ~:# fmap checkFail
                [ "[asb"
                , "asb]"
                ]
            , "bad glob specs" ~:# fmap checkFail
                [ "[:]"
                , "[:a]"
                , "[:*a]"
                , "[:***]"
                ]
            ]

        , "tricksie dixie" ~:#
            [ checkFail "[[]"
            , checkFail "[]]"
            , "[[[]" @@: [lit "[", posGlob]
            , "[]]]" @@: [posGlob, lit "]"]
            ]
        ]

    -- It's a helicopter
    checkFail = either (const $ pure ()) (error "It succeeded")
              . parseOnly sourcePattern
    a @@: b = parseOnly' sourcePattern a @?= SourcePattern b
    namedGlob name = SourceCapture (CaptureNamed name) GlobSingle
    namedGlobStar name = SourceCapture (CaptureNamed name) GlobDouble
    posGlob = SourceCapture CapturePositional GlobSingle
    posGlobStar = SourceCapture CapturePositional GlobDouble
    lit = SourceLiteral

testTargetParsing = tree
  where
    tree =

        [ "single elements" ~:
            [ "literal"           ~: "literal"    @@: [lit "literal"]
            , "named"             ~: "[named]"    @@: [named "named"]
            , "numbered"          ~: "[]"         @@: [pos]
            ]

        , "targets can't have globspecs" ~:# fmap checkFail
            [ "[named:*]"
            , "[named:**]"
            , "[:*]"
            , "[:**]"
            ]

        , "empty strings are okey dokes" ~: "" @@: []

        , "escaping" ~: "ab]]c[[def" @@: [lit "ab]c[def"]

        , "a string with multiple components" ~:
            "so[]me-[example]e"
            @@: [lit "so", pos, lit "me-", named "example", lit "e"]

        , "things that shouldn't compile" ~:
            [ "bad names" ~:# fmap checkFail
                [ "[a1]"
                , "[asbf-sf"
                ]
            , "unpartnered bracket" ~:# fmap checkFail
                [ "[asb"
                , "asb]"
                ]
            , "bad glob specs" ~:# fmap checkFail
                [ "[:]"
                , "[:a]"
                , "[:*a]"
                , "[:***]"
                ]
            ]

        , "tricksie dixie" ~:#
            [ checkFail "[[]"
            , checkFail "[]]"
            , "[[[]" @@: [lit "[", pos]
            , "[]]]" @@: [pos, lit "]"]
            ]
        ]

    -- It's a diver with really big shoes and a waveboard on their back
    checkFail = either (const $ pure ()) (error "It succeeded")
              . parseOnly targetPattern
    a @@: b = parseOnly' targetPattern a @?= TargetPattern b
    named name = TargetCapture (CaptureNamed name)
    pos = TargetCapture CapturePositional
    lit = TargetLiteral


testSubst = tree
  where
    tree =

        [ "literals" ~:#
            [ subst "abc"     "abc"     "abc"     @?= ok "abc"
            , subst "abc"     "repl"    "abc"     @?= ok "repl"
            ]

        , "capture substitution" ~:#
            [ subst "[]"      "[]"      "abc-def" @?= ok "abc-def"
            , subst "[]-[]"   "[]-[]"   "abc-def" @?= ok "abc-def"
            , subst "[a]"     "[a]"     "abc-def" @?= ok "abc-def"
            , subst "[a]-[b]" "[a]-[b]" "abc-def" @?= ok "abc-def"
            , subst "[a]-[b]" "[b]-[a]" "abc-def" @?= ok "def-abc"
            ]

        , "glob" ~:#
            [ subst "a/[]/d"         "x/[]/z"      "a/b/c/d" @?= ng
            , subst "a/[]/[]/d"      "x/[]/[]/z"   "a/b/c/d" @?= ok "x/b/c/z"
            , subst "a/[:**]/d"      "x/[]/z"      "a/b/c/d" @?= ok "x/b/c/z"
            , subst "a/[]/[:**]/e"   "x/[]/[]/z"   "a/b/c/d/e" @?= ok "x/b/c/d/z"
            , subst "a/[:**]/[]/e"   "x/[]/[]/z"   "a/b/c/d/e" @?= ok "x/b/c/d/z"
            ]

        , "bad capture ref" ~:#
            -- FIXME looks like regex is too lenient about these
            [ subst "abc"     "[]"      "abc"     @?= xx -- actual: Just (Just "${1}")
            , subst "abc-[]"  "[]-[]"   "abc-def" @?= xx -- actual: Just (Just "def-${2}")
            -- FIXME this throws an exception straight through us
            , subst "[a]"     "[b]"     "abc"     @?= xx
            ]

        , "emptiez" ~:#
            [ subst "abc"     "abc"     ""        @?= ng
            , subst "abc"     ""        "abc"     @?= ok ""
            , subst ""        "abc"     "abc"     @?= ng
            , subst "abc"     ""        ""        @?= ng
            , subst ""        "abc"     ""        @?= ok "abc"
            , subst ""        ""        "abc"     @?= ng
            ]

        -- NOTE: This documents an unfortunate flaw in the current design,
        --       in that globstars can't match an empty infix if surrounded by '/'.
        --       (a globstar used in a shell does not normally have this issue,
        --        because it generates paths (such as '.') rather than matching)
        , "empty globstar" ~: subst "a/[:**]/b" "a/[]/b" "a/b" @?= ng

        , "syntax error (API test)" ~:#
            [ subst "]" "a" "a" @?= xx
            , subst "a" "]" "a" @?= xx
            ]
        ]

    xx = Nothing      -- Syntax error
    ng = Just Nothing -- No match
    ok = Just . Just  -- Match


propQuotedSubst :: String -> Property
propQuotedSubst s = (not . null) s ==> (s R.=~ re) === s
  where
    Just (Subst re) = compile (quote s)

-- FIXME this property fails on "\n", probably due to regex mode?
propQuotedTemplate :: String -> Property
propQuotedTemplate s = (not . null) s ==> substituted === s
  where
    Just (Just substituted) = subst "[]-[]-[]-[]-[]" repl "a-b-c-d-e"
    repl = quoteTemplate s

substViaSubstInto :: (MonadError String m)=> String -> String -> String -> m (Maybe String)
substViaSubstInto s t u = do
    sub <- compile s
    subInto <- substIntoFunc sub t
    pure $ subInto u

substViaSubstAgainst :: (MonadError String m)=> String -> String -> String -> m (Maybe String)
substViaSubstAgainst s t u =
    compile s >>= \sub ->
      case substAgainstFunc sub u of
        Nothing -> pure Nothing
        Just func -> Just <$> func t

propSubstSubstInto :: String -> String -> String -> Property
propSubstSubstInto s t u =
    -- we're already aware that newlines are broken (FIXME)
    ('\n' `notElem` (s ++ t ++ u))
    ==> left === right
  where
    left :: Either String (Maybe String)
    left = subst s t u
    right = substViaSubstInto s t u

propSubstSubstAgainst :: String -> String -> String -> Property
propSubstSubstAgainst s t u =
    -- we're already aware that newlines are broken (FIXME)
    ('\n' `notElem` (s ++ t ++ u))
    -- subst validates both patterns before attempting to match, while
    -- substAgainst tests matches after only validating one pattern.
    -- This results in an expected discrepancy between the two
    --  for non-matches with invalid replacement patterns.
    && (case (left, right) of (Left _, Right Nothing) -> False
                              _______________________ -> True)
    ==> left === right
  where
    left :: Either String (Maybe String)
    left = subst s t u
    right = substViaSubstAgainst s t u


-- I don't trust that those properties are actually getting any
-- valid capture groups
testSubstIntoCaptures =
    "captures with substViaSubstInto" ~:#
        [ subst "[]"      "[]"      "abc-def" @?= ok "abc-def"
        , subst "[]-[]"   "[]-[]"   "abc-def" @?= ok "abc-def"
        , subst "[a]"     "[a]"     "abc-def" @?= ok "abc-def"
        , subst "[a]-[b]" "[a]-[b]" "abc-def" @?= ok "abc-def"
        , subst "[a]-[b]" "[b]-[a]" "abc-def" @?= ok "def-abc"
        ]
  where subst = substViaSubstInto
        ok = Just . Just

testSubstAgainstCaptures =
    "captures with substViaSubstAgainst" ~:#
        [ subst "[]"      "[]"      "abc-def" @?= ok "abc-def"
        , subst "[]-[]"   "[]-[]"   "abc-def" @?= ok "abc-def"
        , subst "[a]"     "[a]"     "abc-def" @?= ok "abc-def"
        , subst "[a]-[b]" "[a]-[b]" "abc-def" @?= ok "abc-def"
        , subst "[a]-[b]" "[b]-[a]" "abc-def" @?= ok "def-abc"
        ]
  where subst = substViaSubstAgainst
        ok = Just . Just
