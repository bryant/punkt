{-# LANGUAGE OverloadedStrings #-}

module NLP.Hunkt.Match where

import Data.Text (Text)
import Data.Array ((!))
import Text.Regex.TDFA.Text (compile)
import Text.Regex.TDFA (Regex, matchOnceText, blankCompOpt, ExecOption(..))
import Data.Maybe (maybe, catMaybes)
import Data.Either (lefts)

re_split_impl :: Regex -> Text -> [Either Text Text]
re_split_impl re str = filter (/= Left "") $ chunk re str
    where
    chunk re str = maybe [Left str] link $ matchOnceText re str
    link (pre, match, post) = Left pre : Right (fst $ match ! 0) : chunk re post

re_split_with :: Regex -> Text -> (Text -> Maybe a) -> (Text -> Maybe a) -> [a]
re_split_with re str whenmatch whensplit =
    catMaybes $ map (either whenmatch whensplit) (re_split_impl re str)

re_split_pos :: Regex -> Text -> [Either (Text, Int) (Text, Int)]
re_split_pos re str = filter not_blank $ chunk re str 0
    where
    not_blank xs = case xs of { Left ("", _) -> False; _ -> True; }
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

word_seps = re_compile "([ \t\n]+|-{2,}|—|\\.{2,}|\\.( \\.)+|…|[!\\?]{1,})"
intrasep = re_compile "[-'’]"
