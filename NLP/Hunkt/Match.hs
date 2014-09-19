{-# LANGUAGE OverloadedStrings #-}

module NLP.Hunkt.Match where

import Data.Text (Text)
import Data.Array ((!))
import Text.Regex.TDFA.Text (compile)
import Text.Regex.TDFA (Regex, matchOnceText, blankCompOpt, ExecOption(..))

re_split' :: Regex -> Text -> (Text -> Maybe a) -> [Either Text a]
re_split' re str whensplit = case matchOnceText re str of
    Nothing -> [Left str]
    Just (pre, match, post) -> Left pre : case whensplit $ fst (match ! 0) of
        Nothing -> re_split' re post whensplit
        Just mid -> Right mid : re_split' re post whensplit

re_split_with :: Regex -> Text -> (Text -> a) -> (Text -> Maybe a) -> [a]
re_split_with re str whenchunk whensplit = map (either whenchunk id) $ filter not_blank someblanks
    where
    someblanks = re_split' re str whensplit
    not_blank (Left "") = False
    not_blank _ = True

re_split :: Regex -> Text -> [Text]
re_split re str = re_split_with re str id (const Nothing)

re_compile :: Text -> Regex
re_compile re = rv where Right rv = compile blankCompOpt (ExecOption False) re

word_seps = re_compile "([ \t\n]+|-{2,}|—|\\.{2,}|\\.( \\.)+|…|[!\\?]{1,})"
intrasep = re_compile "[-'’]"
