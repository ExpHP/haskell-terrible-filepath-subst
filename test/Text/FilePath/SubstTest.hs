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
    , testProperties
    ]

testProperties :: TestTree
testProperties =
    "Properties" ~:
    [ "Quoted subst" ~: qc propQuotedSubst
    , "Quoted template" ~: qc propQuotedTemplate
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

        [ "simple" ~:#
            [ subst "abc"     "abc"     "abc"     @?= ok "abc"
            , subst "abc"     "repl"    "abc"     @?= ok "repl"
            , subst "[]"      "[]"      "abc-def" @?= ok "abc-def"
            , subst "[]-[]"   "[]-[]"   "abc-def" @?= ok "abc-def"
            , subst "[a]"     "[a]"     "abc-def" @?= ok "abc-def"
            , subst "[a]-[b]" "[a]-[b]" "abc-def" @?= ok "abc-def"
            , subst "[a]-[b]" "[b]-[a]" "abc-def" @?= ok "def-abc"
            , subst "a/[]/d"         "x/[]/z"      "a/b/c/d" @?= ng
            , subst "a/[]/[]/d"      "x/[]/[]/z"   "a/b/c/d" @?= ok "x/b/c/z"
            , subst "a/[:**]/d"      "x/[]/z"      "a/b/c/d" @?= ok "x/b/c/z"
            , subst "a/[]/[:**]/e"   "x/[]/[]/z"   "a/b/c/d/e" @?= ok "x/b/c/d/z"
            , subst "a/[:**]/[]/e"   "x/[]/[]/z"   "a/b/c/d/e" @?= ok "x/b/c/d/z"
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

propQuotedSubst :: String -> Bool
propQuotedSubst s = (null s) || (s R.=~ re) == s
  where
    Just (Subst re) = compile (quote s)

-- FIXME this property fails on "\n", probably due to regex mode?
propQuotedTemplate :: String -> Bool
propQuotedTemplate s = (null s) || substituted == s
  where
    Just (Just substituted) = subst "[]-[]-[]-[]-[]" repl "a-b-c-d-e"
    repl = quoteTemplate s
