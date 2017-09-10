{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleContexts #-}

module NLP.Punkt.Match (
    re_split_impl,
    re_split_pos,
    re_split,
    re_compile,
    word_seps,
    intrasep
    ) where

import Data.Text (Text)
import Data.Array ((!))
import "regex-tdfa-text" Text.Regex.TDFA.Text (compile)
import "regex-tdfa" Text.Regex.TDFA (Regex, matchOnceText, blankCompOpt,
                                     ExecOption(..))
import Data.Either (lefts)

re_split_impl :: Regex -> Text -> [Either Text Text]
re_split_impl re str = filter not_blank $ chunk re str
    where
    not_blank xs = if xs == Left "" || xs == Right "" then False else True
    chunk re str = maybe [Left str] link $ matchOnceText re str
    link (pre, match, post) = Left pre : Right (fst $ match ! 0) : chunk re post

re_split_pos :: Regex -> Text -> [Either (Text, Int) (Text, Int)]
re_split_pos re str = filter not_blank $ chunk re str 0
    where
    not_blank xs =
        case xs of { Left ("", _) -> False; Right ("", _) -> False; _ -> True; }
    chunk re str relpos = case matchOnceText re str of
        Nothing -> [Left (str, relpos)]
        Just (pre, match, post) ->
            let (mtext, (moffset, mlen)) = match ! 0
                (mpos, relpos') = (relpos + moffset, mpos + mlen)
            in Left (pre, relpos) : Right (mtext, mpos) : chunk re post relpos'

re_split :: Regex -> Text -> [Text]
re_split re str = lefts $ re_split_impl re str

re_compile :: Text -> Regex
re_compile re = rv where Right rv = compile blankCompOpt (ExecOption False) re

word_seps, intrasep :: Regex
word_seps = re_compile "([ \t\n]+|-{2,}|—|\\.{2,}|\\.( \\.)+|…|[!\\?;:]{1,})"
intrasep = re_compile "[-'’]"
