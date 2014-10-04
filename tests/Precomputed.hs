{-# LANGUAGE OverloadedStrings #-}

module Precomputed where

import Data.Text as Text (Text, unpack, take, dropAround)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import NLP.Punkt (split_sentences)

infix 4 :=?=>
data SanityCheck = Singleton Text | (:=?=>) Text [Text] deriving Show

test_cases :: [SanityCheck]
test_cases =
    [ Singleton "Look Ma, sentences!"
    , Singleton
        "Computer security experts question that figure, because Apple does \
        \not fully realize how quickly the N.S.A. supercomputers can crack \
        \codes."
    , "The quick brown T. rex swallowed the lazy, then fled from the F.B.I. \
      \Then he disappeared." :=?=> ["The quick brown T. rex swallowed the lazy, then fled from the F.B.I.", "Then he disappeared."]
    ]

precomputed_tests :: TestTree
precomputed_tests = testGroup "Precomputed splits" $ map mkcase test_cases
    where
    mkcase (Singleton s) = testCase (prefix s) $ split_sentences s @?= [s]
    mkcase (s :=?=> xs) = testCase (prefix s) $ split_sentences s @?= xs
    prefix = Text.unpack . Text.dropAround (== ' ') . Text.take 24
