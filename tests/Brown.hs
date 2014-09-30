{-# LANGUAGE OverloadedStrings #-}

module Brown where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import NLP.Punkt.Match (re_compile)
import NLP.Punkt
import Text.Regex.TDFA (matchOnce)
import qualified Control.Monad.Reader as Reader

data Tagged
    = TWord Text Text
    | Ender Text
    | Misc Text Text
    deriving (Eq, Show)

is_misc (Misc _ _) = True
is_misc _ = False

list_corpora :: IO [FilePath]
list_corpora = proc `fmap` readFile "./corpora/brown/cats.txt"
    where proc = map head . map words . lines

read_corp codename = fmap Text.pack $ readFile ("./corpora/brown/" ++ codename)

parse_corpus = map (to_tok . Text.breakOnEnd "/") . Text.words
    where
    to_tok (w, attr)
        | Text.find (== '.') attr /= Nothing = Ender $ Text.init w
        | otherwise = ctor (Text.init w) attr
        where ctor = if non_word attr then Misc else TWord
    non_word = isJust . matchOnce non_re
    -- is_word = isJust . matchOnce word_re

non_re = re_compile "(-{2,}|\\(|\\)|,|:|`{1,}|'{1,})"
word_re = re_compile "[A-Za-z0-9\\$\\*]"

ctx_filter :: Int -> (a -> Bool) -> [a] -> [[a]]
ctx_filter n match xs = map (\k -> subseg (k-n) (k+n) xs) matches
    where
    matches = map snd . filter (match . fst) $ zip xs [0..]
    subseg m n = take (n - m) . drop m

to_punkt_toks :: [Tagged] -> [Token]
to_punkt_toks = map to_punkt . group_enders . filter (not . is_misc)
    where
    group_enders [] = []
    group_enders (TWord w _ : Ender "." : toks) = Ender w' : group_enders toks
        where
        w' = (if Text.last w == '.' then Text.init w else w) `Text.snoc` '.'
    group_enders (tok:toks) = tok : group_enders toks

    to_punkt (Ender w) = Token 0 0 (Word w) True False
    to_punkt (TWord w _) = Token 0 0 (Word w) False False

data PunktError
    = FalseNegative Token
    | FalsePositive Token
    | Awesome Token
    deriving Show

is_err (Awesome _) = False
is_err _ = True

false_pos (FalsePositive _) = True
false_pos _ = False

false_neg (FalseNegative _) = True
false_neg _ = False

erate reftoks results = (flength fpos + flength fneg) / flength candidates
    where
    flength = fromIntegral . length
    fpos = filter false_pos results
    fneg = filter false_neg results
    candidates = filter sentend reftoks

bench :: [Token] -> ([Token] -> [Token]) -> [PunktError]
bench reftoks algo = zipWith judge reftoks (algo toks)
    where
    toks = map mask reftoks where mask tok = tok { sentend = False }
    judge ref actual = judge' ref actual actual
    judge' ref actual
        | sentend ref && not (sentend actual) = FalseNegative
        | not (sentend ref) && sentend actual = FalsePositive
        | otherwise = Awesome

control :: [Token] -> [Token]
control = map control'
    where
    control' tok@(Token {entity=(Word w)})
        | Text.last w == '.' || Text.head w `elem` ":;?!" =
            tok { sentend = True }
    control' tok = tok { sentend = False }

punkt :: [Token] -> [Token]
punkt toks = runPunkt (build_punkt_data toks) $ do
        abbrs <- mapM classify_by_type toks
        Reader.zipWithM classify_by_next abbrs (drop 1 abbrs)

main = do
    corpi <- list_corpora >>= mapM (fmap Text.pack . readFile . ("./corpora/brown/"++))
    let ref = to_punkt_toks $ concatMap parse_corpus corpi
    let punkterr = 100 * erate ref (bench ref punkt)
    let ctrlerr = 100 * erate ref (bench ref control)
    putStrLn $ "error rates: punkt = " ++ show punkterr ++ "%, control = "
               ++ show ctrlerr ++ "%"

