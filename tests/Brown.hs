{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Brown where

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import NLP.Punkt.Match (re_compile)
import NLP.Punkt
import "regex-tdfa" Text.Regex.TDFA (matchOnce)
import qualified Control.Monad.Reader as Reader
import Control.Applicative ((<$>), (<*>))
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, assertFailure)

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

read_corp :: FilePath -> IO Text
read_corp codename = fmap Text.pack $ readFile ("./corpora/brown/" ++ codename)

parse_corpus :: Text -> [Tagged]
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
to_punkt_toks = filter (not . empty . entity) . to_punkt 0 . group_enders
              . filter (not . is_misc)
    where
    group_enders [] = []
    group_enders (TWord w _ : Ender "." : toks) = Ender w' : group_enders toks
        where
        w' = (if Text.last w == '.' then Text.init w else w) `Text.snoc` '.'
    group_enders (tok:toks) = tok : group_enders toks

    to_punkt n [] = []
    to_punkt n (Ender w : xs) = Token n 0 (pword w) True False : to_punkt (n+1) xs
    to_punkt n (TWord w _ : xs) = Token n 0 (pword w) False False : to_punkt (n+1) xs

    pword w = Word (if p then Text.init w else w) p where p = Text.last w == '.'

    empty (Word "" _) = True
    empty _ = False

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

errs reftoks results = (fpos, fneg, tpos)
    where
    flength = fromIntegral . length
    fpos = flength $ filter false_pos results
    fneg = flength $ filter false_neg results
    tpos = flength $ filter fff results
        where
        fff (Awesome tok) = sentend tok
        fff _ = False

mask tok = tok { sentend = False }

bench :: [Token] -> ([Token] -> [Token]) -> [PunktError]
bench reftoks algo = zipWith judge reftoks (algo toks)
    where
    toks = map mask reftoks
    judge ref actual
        | sentend ref && not (sentend actual) = FalseNegative actual
        | not (sentend ref) && sentend actual = FalsePositive actual
        | otherwise = Awesome actual

control :: [Token] -> [Token]
control = map control'
    where
    control' tok@(Token {entity=(Word w period_end)})
        | period_end || Text.head w `elem` (":;?!" :: [Char]) = tok { sentend = True }
    control' tok = tok { sentend = False }

punkt :: [Token] -> [Token]
punkt toks = runPunkt (build_punkt_data toks') $ do
    abbrs <- mapM classify_by_type toks'
    Reader.zipWithM classify_by_next abbrs (drop 1 abbrs)
    where toks' = map punktlexsim toks

punktlexsim tok@(Token {entity=Word w _})
    | Text.head w `elem` (":;?!" :: [Char]) = tok { entity = Punct w, sentend = True }
    | otherwise = tok

benchmark_brown :: TestTree
benchmark_brown = testCase "Brown corpus" $ do
    corpi <- list_corpora >>= mapM (fmap Text.pack . readFile . ("./corpora/brown/"++))
    let ref = to_punkt_toks $ concatMap parse_corpus corpi
    let (punktfp, punktfn, punkttp) = errs ref (bench ref punkt)
    let (ctrlfp, ctrlfn, ctrltp) = errs ref (bench ref control)
    let punkterr = erate punktfp punktfn punkttp
    let ctrlerr = erate ctrlfp ctrlfn ctrltp


    {-putStrLn $ "Error rates: Punkt = " ++ show (punktfp, punktfn, punkttp) ++
               " = " ++ show punkterr ++ "%, Control = " ++
               show (ctrlfp, ctrlfn, ctrltp) ++ " = " ++ show ctrlerr ++ "%"
    -}

    -- this number is completely arbitrary.
    if punkterr > 1.20
        then assertFailure $ show punkterr ++ " exceeds 1.20% threshold."
        else return ()
    where erate fp fn tp = 100 * (fp + fn) / (fp + fn + tp)
