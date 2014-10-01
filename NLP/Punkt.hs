{-# LANGUAGE OverloadedStrings #-}

module NLP.Punkt where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import Data.Char (isLower, isAlpha)
import qualified Data.Map as Map
import qualified Data.List as List
import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.Reader as Reader

import NLP.Punkt.Match (re_split, re_split_pos, intrasep, word_seps)

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
    collocations :: Map (Text, Text) Int,
    total_enders :: Int,
    total_toks :: Int
    }
    deriving Show

data Entity a = Word a Bool | Punct a | ParaStart | Ellipsis | Dash
    deriving (Eq, Show)

data Token = Token {
    offset :: Int,
    toklen :: Int,
    entity :: Entity Text,
    sentend :: Bool,
    abbrev :: Bool
    }
    deriving Show

type Punkt = Reader.Reader PunktData

norm :: Text -> Text
norm = Text.toLower

is_initial :: Token -> Bool
is_initial (Token {entity=Word w True}) =
    Text.length w == 1 && isAlpha (Text.head w)
is_initial _ = False

is_word :: Token -> Bool
is_word tok = case entity tok of { Word _ _ -> True; _ -> False; }

-- dunning log likelihood modified by kiss/strunk
strunk_log :: Double -> Double -> Double -> Double -> Double
strunk_log a b ab n = -2 * (null - alt)
    where
    null = ab * log p1 + (a - ab) * log (1 - p1)
    alt = ab * log p2 + (a - ab) * log (1 - p2)
    (p1, p2) = (b / n, 0.99)

-- vanilla dunning log likelihood
dunning_log :: Double -> Double -> Double -> Double -> Double
dunning_log a b ab n | b == 0 || ab == 0 = 0
                     | otherwise = -2 * (s1 + s2 - s3 - s4)
    where
    (p0, p1, p2) = (b / n, ab / a, (b - ab) / (n - a))
    s1 = ab * log p0 + (a - ab) * log (1 - p0)
    s2 = (b - ab) * log p0 + (n - a - b + ab) * log (1 - p0)
    s3 = if a == ab then 0 else ab * log p1 + (a - ab) * log (1 - p1)
    s4 = if b == ab then 0 else
        (b - ab) * log p2 + (n - a - b + ab) * log (1 - p2)

ask_type_count = Reader.liftM type_count Reader.ask
ask_total_toks = Reader.liftM (fromIntegral . total_toks) Reader.ask
ask_total_enders = Reader.liftM (fromIntegral . total_enders) Reader.ask

ask_ortho :: Text -> Punkt OrthoFreq
ask_ortho w_ = return . Map.findWithDefault (OrthoFreq 0 0 0 0 0) (norm w_)
               =<< fmap ortho_count Reader.ask

ask_colloc :: Text -> Text -> Punkt Double
ask_colloc w0_ w1_ =
    return . fromIntegral . Map.findWithDefault 0 (norm w0_, norm w1_)
    =<< collocations <$> Reader.ask

-- c(w, ~.)
freq :: Text -> Punkt Double
freq w_ = ask_type_count >>= return . fromIntegral . Map.findWithDefault 0 w
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
prob_abbr :: Text -> Punkt Double
prob_abbr w_ = compensate =<< strunk_log <$> freq_type w_ <*> freq "."
                                         <*> freq_snoc_dot w_ <*> ask_total_toks
    where
    compensate loglike = do
        f_penalty <- do
            p <- freq w_  -- c(w, ~.)
            return $ 1 / dlen (Text.filter (/= '.') w_) ** p
        return $ loglike * f_len * f_periods * f_penalty
    f_len = 1 / exp (dlen $ Text.filter (/= '.') w_)
    f_periods = 1 + dlen (Text.filter (== '.') w_)

-- decides if w is a sentence ender based on its capitalization
decide_ortho :: Text -> Punkt (Maybe Bool)
decide_ortho w_ = do
    orthofreq <- ask_ortho w_
    let occurs_upper = freq_upper orthofreq > 0
    let occurs_lower = freq_lower orthofreq > 0
    let never_internal_upper = freq_internal_upper orthofreq == 0
    let never_first_lower = freq_first_lower orthofreq == 0
    let rv | not lower && occurs_lower && never_internal_upper = Just True
           | lower && (occurs_upper || never_first_lower) = Just False
           | otherwise = Nothing
    return rv
    where lower = isLower $ Text.head w_

-- probability that w_ is a frequent sentence starter
prob_starter :: Text -> Punkt Double
prob_starter w_ = dunning_log <$> ask_total_enders <*> freq_type w_
                              <*> fafterend <*> ask_total_toks
    where fafterend = fromIntegral . freq_after_ender <$> ask_ortho w_

prob_colloc :: Text -> Text -> Punkt Double
prob_colloc w_ x_ = dunning_log <$> freq_type w_ <*> freq_type x_
                                <*> ask_colloc w_ x_ <*> ask_total_toks

build_type_count :: [Token] -> Map Text Int
build_type_count = List.foldl' update initcount
    where
    initcount = Map.singleton "." 0

    update ctr (Token {entity=(Word w per)})
        | per = Map.update (\n -> Just $ n + 1) "." ctr_
        | otherwise = ctr_
        where
        ctr_ = Map.insertWith (\_ n -> n + 1) wnorm 1 ctr
        wnorm = norm $ if per then w `Text.snoc` '.' else w
    update ctr _ = ctr

    -- TODO: catch possible abbreviations wrapped in hyphenated and apostrophe
    -- forms in lexer

build_ortho_count :: [Token] -> Map Text OrthoFreq
build_ortho_count toks = List.foldl' update Map.empty $
                            zip (dummy : toks) toks
    where
    dummy = Token 0 0 (Word " " False) True False
    -- hack: add dummy to process first token

    update :: Map Text OrthoFreq -> (Token, Token) -> Map Text OrthoFreq
    update ctr (prev, Token {entity=(Word w _)}) = Map.insert wnorm wortho ctr
        where
        upd (OrthoFreq a b c d e) a' b' c' d' e' =
            OrthoFreq (a |+ a') (b |+ b') (c |+ c') (d |+ d') (e |+ e')
            where int |+ bool = if bool then int + 1 else int

        wortho = upd z lower (not lower) (first && lower)
                       (internal && not lower) first
        z = Map.findWithDefault (OrthoFreq 0 0 0 0 0) wnorm ctr
        wnorm = norm w
        lower = isLower $ Text.head w
        first = sentend prev && not (is_initial prev)
        internal = not (sentend prev) && not (abbrev prev)
                   && not (is_initial prev)
    update ctr _ = ctr

build_collocs :: [Token] -> Map (Text, Text) Int
build_collocs toks = List.foldl' update Map.empty $ zip toks (drop 1 toks)
    where
    update ctr (Token {entity=(Word u _)}, Token {entity=(Word v _)}) =
        Map.insertWith (+) (norm u, norm v) 1 ctr
    update ctr _ = ctr

to_tokens :: Text -> [Token]
to_tokens corpus = catMaybes . map (either tok_word add_delim) $
                        re_split_pos word_seps corpus
    where
    tok_word (w, pos)
        | trim == "" = Nothing
        | otherwise = Just $ Token pos (len trim) (Word s period) False False
        where
        trim = Text.dropAround (`elem` ",:()[]{}“”’\"\')") w
        period = Text.last trim == '.'
        s = if period then Text.init trim else trim

    add_delim (delim, pos)
        | d `elem` "—-" = Just $ Token pos (len delim) Dash False False
        | d `elem` ".…" = Just $ Token pos (len delim) Ellipsis False False
        | d `elem` ";!?" = Just $ Token pos (len delim) (Punct delim) True False
        | otherwise = Nothing
        where d = Text.head delim

    len = Text.length

build_punkt_data :: [Token] -> PunktData
build_punkt_data toks = PunktData typecnt orthocnt collocs nender totes
    where
    typecnt = build_type_count toks
    temppunkt = PunktData typecnt Map.empty Map.empty 0 (length toks)
    refined = Reader.runReader (mapM classify_by_type toks) temppunkt
    orthocnt = build_ortho_count refined
    collocs = build_collocs refined
    nender = length . filter (sentend . fst) $ zip (dummy : refined) refined
    dummy = Token 0 0 (Word " " False) True False
    totes = length $ filter is_word toks

classify_by_type :: Token -> Punkt Token
classify_by_type tok@(Token {entity=(Word w True)}) = do
    p <- prob_abbr w
    return $ tok { abbrev = p >= 0.3, sentend = p < 0.3}
classify_by_type tok = return tok

classify_by_next :: Token -> Token -> Punkt Token
classify_by_next this (Token _ _ (Word next _) _ _)
    | not (is_initial this) && (entity this == Ellipsis || abbrev this) = do
        ortho_says <- decide_ortho next
        prob_says <- prob_starter next
        return $ case ortho_says of
            Nothing -> this { sentend = prob_says >= 30 }
            Just bool -> this { sentend = bool }
classify_by_next this _ = return this

classify_initials :: Token -> Token -> Punkt Token
-- only search for possible initials followed by a word type
classify_initials itok@(Token {entity=Word i True}) (Token {entity=Word next _})
    | is_initial itok = do
        colo <- prob_colloc i next
        startnext <- prob_starter next
        orthonext <- decide_ortho next
        case colo >= 7.88 && startnext < 30 of
            -- TODO: simplify this series of checks into its own collocational
            -- version of decide_ortho
            False -> case orthonext of
                Nothing -> do
                    next_never_lower <- (== 0) . freq_lower <$> ask_ortho next
                    return $ if next_never_lower then itok_is_abbrev else itok
                Just False -> return itok_is_abbrev
                Just True -> return itok  -- never reclassify as sentend
            True -> return itok_is_abbrev
    where itok_is_abbrev = itok { abbrev = True, sentend = False }
classify_initials itok _ = return itok

find_breaks :: Text -> [Int]
find_breaks corpus = Reader.runReader find_breaks punkt
    where
    toks = to_tokens corpus
    punkt = build_punkt_data toks
    find_breaks = do
        abbrd <- mapM classify_by_type toks
        final <- Reader.zipWithM classify_by_next abbrd (drop 1 abbrd)
        return $ map (\t -> offset t + toklen t) (filter sentend final)

chop :: [Int] -> Text -> [Text]
chop offsets text = zipWith (substr text) offsets (drop 1 offsets)
    where substr text m n = Text.take (n - m) $ Text.drop m text

split_sentences :: Text -> [Text]
split_sentences corpus = chop (0 : breaks ++ [Text.length corpus - 1]) corpus
    where breaks = find_breaks corpus

runPunkt :: PunktData -> Punkt a -> a
runPunkt = flip Reader.runReader
