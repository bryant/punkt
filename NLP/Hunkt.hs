{-# LANGUAGE OverloadedStrings #-}

module NLP.Hunkt where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Map (Map)
import Data.Char (isLower)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.Reader as Reader

import NLP.Hunkt.Match

data OrthoFreq = OrthoFreq {
    freq_lower :: Int,
    freq_upper :: Int,
    freq_first_lower :: Int,
    freq_internal_upper :: Int,
    freq_after_ender :: Int
    }
    deriving Show

data PunktData = PunktData {
    type_count :: Map Text Int,  -- abbreviation counter
    ortho_count :: Map Text OrthoFreq,
    total_enders :: Int,
    total_toks :: Int
    }
    deriving Show

data Entity a = Word a | ParaStart | Ellipsis | Dash
    deriving (Eq, Show)

data Token = Token {
    entity :: Entity Text,
    sentend :: Bool,
    abbrev :: Bool
    }
    deriving Show

type Punkt = Reader.Reader PunktData

norm :: Text -> Text
norm = Text.toLower

-- dunning log likelihood modified by kiss/strunk
strunk_log :: Double -> Double -> Double -> Double -> Double
strunk_log a b ab n = -2 * (null - alt)
    where
    null = ab * log p1 + (a - ab) * log (1 - p1)
    alt = ab * log p2 + (a - ab) * log (1 - p2)
    (p1, p2) = (b / n, 0.99)

-- vanilla dunning log likelihood
dunning_log :: Double -> Double -> Double -> Double -> Double
dunning_log a b ab n = -2 * (s1 + s2 - s3 - s4)
    where
    (p0, p1, p2) = (b / n, ab / a, (b - ab) / (n - a))
    s1 = ab * log p0 + (a - ab) * log (1 - p0)
    s2 = (b - ab) * log p0 + (n - a - b + ab) * log (1 - p0)
    s3 = if a == ab then 0 else ab * log p1 + (a - ab) * log (1 - p1)
    s4 = if b == ab then 0 else
        (b - ab) * log p2 + (n - a - b + ab) * log (1 - p2)

-- c(w, ~.)
freq :: Text -> Punkt Double
freq w_ = fmap type_count Reader.ask >>=
            return . fromIntegral . Map.findWithDefault 0 w
    where w = norm w_

-- c(w, .)
freq_snoc_dot :: Text -> Punkt Double
freq_snoc_dot w_ = freq wdot where wdot = w_ `Text.snoc` '.'
-- potential slowdown if ghc doesn't know that norm "." == "."

-- c(w) == c(w, .) + c(w, ~.)
freq_type :: Text -> Punkt Double
freq_type w_ = (+) <$> freq w_ <*> freq_snoc_dot w_

dlen :: Text -> Double
dlen = fromIntegral . Text.length

-- probability that (w_ `snoc` '.') is an abbreviation.
prob_abbr :: Map Text Int -> Int -> Text -> Double
prob_abbr ctr ntoks w_ = log_like * f_len * f_periods * f_penalty
    where
    log_like = strunk_log (count wp + count w) (count ".")
                          (count wp) (fromIntegral ntoks)
    w = norm w_
    wp = w `Text.snoc` '.'

    f_len = 1 / exp (len $ Text.filter (/= '.') w)
    f_periods = 1 + len (Text.filter (== '.') w)
    f_penalty = 1 / len (Text.filter (/= '.') w) ^ count w

    len = fromIntegral . Text.length
    count w = fromIntegral $ Map.findWithDefault 0 w ctr

-- decides if w is a sentence ender based on its capitalization
decide_ortho :: Map Text OrthoFreq -> Text -> Maybe Bool
decide_ortho ctr w_
    | upper && occurs_lower && never_internal_upper = Just True
    | lower && (occurs_upper || never_first_lower) = Just False
    | otherwise = Nothing
    where
    upper = not lower
    lower = isLower $ Text.head w_

    occurs_upper = freq_upper orthofreq > 0
    occurs_lower = freq_lower orthofreq > 0
    never_internal_upper = freq_internal_upper orthofreq == 0
    never_first_lower = freq_first_lower orthofreq == 0
    orthofreq = Map.findWithDefault (OrthoFreq 0 0 0 0 0) (norm w_) ctr

-- probability that w_ is a frequent sentence starter
prob_starter :: Map Text Int -> Map Text OrthoFreq -> Int -> Int -> Text ->
                Double
prob_starter abbrctr orthoctr nenders ntoks w_
    | count w == 0 || count_w_start == 0 = 0
    | otherwise = dunning_log (fromIntegral nenders) (count w + count wp)
                              count_w_start (fromIntegral ntoks)
    where
    w = norm w_
    wp = w `Text.snoc` '.'

    count w = fromIntegral $ Map.findWithDefault 0 w abbrctr
    count_w_start = fromIntegral $ freq_after_ender orthofreq
    orthofreq = Map.findWithDefault (OrthoFreq 0 0 0 0 0) w orthoctr

build_type_count :: [Token] -> Map Text Int
build_type_count = List.foldl' update initcount
    where
    initcount = Map.singleton "." 0

    update ctr (Token {entity=(Word w)})
        | last_per w = Map.update (\n -> Just $ n + 1) "." ctr_
        | otherwise = ctr_
        where
        ctr_ = Map.insertWith (\_ n -> n + 1) wnorm 1 ctr
        wnorm = norm $ extract_abbr w
    update ctr _ = ctr

    -- catch possible abbreviations wrapped in hyphenated and apostrophe forms
    extract_abbr w_ = case filter last_per (re_split intrasep w_) of
        [] -> w_
        w_ : _ -> w_  -- dubious

    intrasep = re_compile "[-'’]"
    last_per w = Text.last w == '.'

build_ortho_count :: [Token] -> Map Text OrthoFreq
build_ortho_count toks = List.foldl' update Map.empty $
                            zip (dummy : toks) toks
    where
    dummy = Token (Word " ") True False
    -- hack: add dummy to process first token

    update :: Map Text OrthoFreq -> (Token, Token) -> Map Text OrthoFreq
    update ctr (prev, Token {entity=(Word w)}) = Map.insert wnorm wortho ctr
        where
        upd (OrthoFreq a b c d e) a' b' c' d' e' =
            OrthoFreq (a |+ a') (b |+ b') (c |+ c') (d |+ d') (e |+ e')
            where int |+ bool = if bool then int + 1 else int

        wortho = upd z lower (not lower) (first && lower)
                       (not first && not lower) first
        z = Map.findWithDefault (OrthoFreq 0 0 0 0 0) wnorm ctr
        wnorm = norm w
        lower = isLower $ Text.head w
        first = sentend prev
    update ctr _ = ctr

to_tokens :: Text -> [Token]
to_tokens corpus = re_split_with word_seps corpus tok_word add_delim
    where
    word_seps = re_compile "([ \t\n]+|-{2,}|—|\\.{2,}|\\.( \\.)+|…|[!\\?]{1,})"

    tok_word w = Token (Word stripped) False False
        where stripped = Text.dropAround (`elem` ",;:()[]{}“”’\"\')") w

    add_delim delim | d `elem` "—-" = Just $ Token Dash False False
                    | d `elem` ".…" = Just $ Token Ellipsis False False
                    | d `elem` "!?" = Just $ Token (Word delim) True False
                    | otherwise = Nothing
        where d = Text.head delim

build_punkt_data :: Text -> PunktData
build_punkt_data corpus = PunktData typecnt orthocnt nender (length toks)
    where
    toks = to_tokens corpus
    typecnt = build_type_count toks
    temppunkt = PunktData typecnt Map.empty 0 (length toks)
    refined = Reader.runReader (mapM classify_by_type toks) temppunkt
    orthocnt = build_ortho_count refined
    nender = length . filter (sentend . fst) $ zip (dummy : refined) refined
    dummy = Token (Word " ") True False

classify_by_type :: Token -> Punkt Token
classify_by_type tok@(Token {entity=(Word w)})
    | Text.last w == '.' = do
        p <- prob_abbr <$> fmap type_count Reader.ask
                       <*> fmap total_toks Reader.ask <*> return (Text.init w)
        return $ tok { abbrev = p >= 0.3, sentend = p < 0.3}
    | otherwise = return tok
classify_by_type tok = return tok

classify_by_next :: (Token, Token) -> Punkt Token
classify_by_next (this, Token (Word next) _ _)
    | entity this == Ellipsis || abbrev this = do
        PunktData typecnt orthocnt nenders ntoks <- Reader.ask
        return $ case decide_ortho orthocnt next of
            Just bool -> this { sentend = bool }
            Nothing -> do
                this { sentend =
                    prob_starter typecnt orthocnt nenders ntoks next >= 30 }
classify_by_next (this, _) = return this

coeur :: Text -> [Token]
coeur corpus = Reader.runReader c punkt
    where
    toks = to_tokens corpus
    punkt = build_punkt_data corpus
    c = mapM classify_by_type toks >>= mapM classify_by_next . to_pairs
    to_pairs xs = zip xs $ drop 1 xs

{-
--split_sentences :: Text -> Text
split_sentences corpus =
    let
    stats = build_punkt_data corpus
    toks =
        map (refine
        map (classify_by_type (type_count stats) (total_toks stats)) $
        to_tokens corpus
    in
    toks
-}
