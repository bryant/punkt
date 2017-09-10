{-# LANGUAGE OverloadedStrings #-}

module NLP.Punkt where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Maybe (catMaybes, fromMaybe)
import Data.HashMap.Strict (HashMap)
import Data.Char (isLower, isAlpha, isSpace)
import qualified Data.HashMap.Strict as Map
import qualified Data.List as List
import Control.Applicative ((<$>), (<*>), (<|>))
import qualified Control.Monad.Reader as Reader

import NLP.Punkt.Match (re_split_pos, word_seps)

-- | Carries various orthographic statistics for a particular textual type.
data OrthoFreq = OrthoFreq {
    freq_lower :: Int,
    -- ^ number of lowercase occurrences
    freq_upper :: Int,
    -- ^ uppercase occurrences
    freq_first_lower :: Int,
    -- ^ number of lowercase occurrences in the first position of a sentence
    freq_internal_upper :: Int,
    -- ^ number of uppercase occurrences strictly internal to a sentence
    freq_after_ender :: Int
    -- ^ number of occurences in the first position
    }
    deriving Show

-- | Represents training data obtained from a corpus required by Punkt.
data PunktData = PunktData {
    type_count :: HashMap Text Int,
    -- ^ Occurrences of each textual type, case-insensitive. Used during Punkt's
    -- type-based stage. Also contains occurrences of trailing periods.
    ortho_count :: HashMap Text OrthoFreq,
    -- ^ Dictionary of orthographic data for each textual type.
    collocations :: HashMap (Text, Text) Int,
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

-- | Dunning log likelihood modified by Kiss/Strunk
strunk_log :: Double -> Double -> Double -> Double -> Double
strunk_log a b ab n = -2 * (null - alt)
    where
    null = ab * log p1 + (a - ab) * log (1 - p1)
    alt = ab * log p2 + (a - ab) * log (1 - p2)
    (p1, p2) = (b / n, 0.99)

-- | Dunning's original log likelihood
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

ask_type_count :: Punkt (HashMap Text Int)
ask_type_count = Reader.liftM type_count Reader.ask

ask_total_toks :: Num a => Punkt a
ask_total_toks = Reader.liftM (fromIntegral . total_toks) Reader.ask

ask_total_enders :: Num a => Punkt a
ask_total_enders = Reader.liftM (fromIntegral . total_enders) Reader.ask

ask_ortho :: Text -> Punkt OrthoFreq
ask_ortho w_ = return . Map.lookupDefault (OrthoFreq 0 0 0 0 0) (norm w_)
               =<< fmap ortho_count Reader.ask

ask_colloc :: Text -> Text -> Punkt Double
ask_colloc w0_ w1_ =
    return . fromIntegral . Map.lookupDefault 0 (norm w0_, norm w1_)
    =<< collocations <$> Reader.ask

-- | Occurrences of a textual type, strictly ignoring trailing period.
-- @c(w, ~.)@. Case-insensitive.
freq :: Text -> Punkt Double
freq w_ = ask_type_count >>= return . fromIntegral . Map.lookupDefault 0 w
    where w = norm w_

-- | Occurrences of a textual type with trailing period. @c(w, .)@.
-- Case-insensitive.
freq_snoc_dot :: Text -> Punkt Double
freq_snoc_dot w_ = freq wdot where wdot = w_ `Text.snoc` '.'
-- potential slowdown if ghc doesn't know that norm "." == "."

-- | @c(w) == c(w, .) + c(w, ~.)@. Case-insensitive.
freq_type :: Text -> Punkt Double
freq_type w_ = (+) <$> freq w_ <*> freq_snoc_dot w_

dlen :: Text -> Double
dlen = fromIntegral . Text.length

-- | Returns the log likelihood that (w_ `snoc` '.') is an abbreviation.
-- Case-insensitive.
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

-- | Decides if @w@ is a sentence ender based on its capitalization.
-- Case-insensitive.
decide_ortho :: Text -> Punkt (Maybe Bool)
decide_ortho w_ = ask_ortho w_ >>= return . decide' w_
    where
    decide' w_ wortho
        | title && ever_lower && never_title_internal = Just True
        | lower && (ever_title || never_lower_start) = Just False
        | otherwise = Nothing
        where
        (lower, title) = (isLower $ Text.head w_, not lower)
        ever_lower = freq_lower wortho > 0
        never_title_internal = freq_internal_upper wortho == 0
        ever_title = freq_upper wortho > 0
        never_lower_start = freq_first_lower wortho == 0

-- | Special orthographic heuristic for post-possible-initial tokens.
-- Case-insensitive.
decide_initial_ortho :: Text -> Punkt (Maybe Bool)
decide_initial_ortho w_ = do
    neverlower <- (== 0) . freq_lower <$> ask_ortho w_
    orthosays <- decide_ortho w_
    return $ orthosays <|> if neverlower then Just False else Nothing

-- | Log likelihood that @w@ is a frequent sentence starter. Case-insensitive.
prob_starter :: Text -> Punkt Double
prob_starter w_ = dunning_log <$> ask_total_enders <*> freq_type w_
                              <*> fafterend <*> ask_total_toks
    where fafterend = fromIntegral . freq_after_ender <$> ask_ortho w_

-- | Computes the collocational likelihood of @w@ and @x@. Case-insensitive.
prob_colloc :: Text -> Text -> Punkt Double
prob_colloc w_ x_ = dunning_log <$> freq_type w_ <*> freq_type x_
                                <*> ask_colloc w_ x_ <*> ask_total_toks

-- | Builds a dictionary of textual type frequencies from a stream of tokens.
build_type_count :: [Token] -> HashMap Text Int
build_type_count = List.foldl' update initcount
    where
    initcount = Map.singleton "." 0

    update ctr (Token {entity=(Word w per)})
        | per = Map.adjust (+ 1) "." ctr_
        | otherwise = ctr_
        where
        ctr_ = Map.insertWith (+) wnorm 1 ctr
        wnorm = norm $ if per then w `Text.snoc` '.' else w
    update ctr _ = ctr

    -- TODO: catch possible abbreviations wrapped in hyphenated and apostrophe
    -- forms in lexer

build_ortho_count :: [Token] -> HashMap Text OrthoFreq
build_ortho_count toks = List.foldl' update Map.empty $
                            zip (dummy : toks) toks
    where
    dummy = Token 0 0 (Word " " False) True False
    -- hack: add dummy to process first token

    update :: HashMap Text OrthoFreq -> (Token, Token) -> HashMap Text OrthoFreq
    update ctr (prev, Token {entity=(Word w _)}) = Map.insert wnorm wortho ctr
        where
        upd (OrthoFreq a b c d e) a' b' c' d' e' =
            OrthoFreq (a |+ a') (b |+ b') (c |+ c') (d |+ d') (e |+ e')
            where int |+ bool = if bool then int + 1 else int

        wortho = upd z lower (not lower) (first && lower)
                       (internal && not lower) first
        z = Map.lookupDefault (OrthoFreq 0 0 0 0 0) wnorm ctr
        wnorm = norm w
        lower = isLower $ Text.head w
        first = sentend prev && not (is_initial prev)
        internal = not (sentend prev) && not (abbrev prev)
                   && not (is_initial prev)
    update ctr _ = ctr

build_collocs :: [Token] -> HashMap (Text, Text) Int
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
        trim = Text.dropAround (`elem` symbols) w
          where
            symbols = ",:()[]{}“”’\"\')" :: String
        period = Text.last trim == '.'
        s = if period then Text.init trim else trim

    add_delim (delim, pos)
        | d `elem` ("—-" :: String) = Just $ Token pos (len delim) Dash False False
        | d `elem` (".…" :: String) = Just $ Token pos (len delim) Ellipsis False False
        | d `elem` (";!?" :: String) = Just $ Token pos (len delim) (Punct delim) True False
        | otherwise = Nothing
        where  d = Text.head delim

    len = Text.length

build_punkt_data :: [Token] -> PunktData
build_punkt_data toks = PunktData typecnt orthocnt collocs nender totes
    where
    typecnt = build_type_count toks
    temppunkt = PunktData typecnt Map.empty Map.empty 0 (length toks)
    refined = runPunkt temppunkt $ mapM classify_by_type toks
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
    | is_initial this = do
        let Word thisinitial _ = entity this
        colo <- prob_colloc thisinitial next
        startnext <- prob_starter next
        orthonext <- decide_initial_ortho next
        return $ if (colo >= 7.88 && startnext < 30) || orthonext == Just False
            then this { abbrev = True, sentend = False}
            else this  -- never reclassify as sentend
    | entity this == Ellipsis || abbrev this = do
        ortho_says <- decide_ortho next
        prob_says <- prob_starter next
        return $ case ortho_says of
            Nothing -> this { sentend = prob_says >= 30 }
            Just bool -> this { sentend = bool }
classify_by_next this _ = return this

classify_punkt :: Text -> [Token]
classify_punkt corpus = runPunkt (build_punkt_data toks) $ do
    abbrd <- mapM classify_by_type toks
    final <- Reader.zipWithM classify_by_next abbrd (drop 1 abbrd)
    return $ final ++ [last toks]
    where toks = to_tokens corpus

find_breaks :: Text -> [(Int, Int)]
find_breaks corpus = slices_from endpairs 0
    where
    pairs_of xs = zip xs $ drop 1 xs
    endpairs = filter (sentend . fst) . pairs_of $ classify_punkt corpus

    -- TODO: make this less convoluted
    slices_from [] n = [(n, Text.length corpus)]
    slices_from ((endtok, nexttok):pairs) n = (n, endpos + end) : slices_from pairs (endpos + n')
        where
        endpos = offset endtok + toklen endtok
        (end, n') = fromMaybe (endpos, endpos + 1) . match_spaces $
            substring corpus endpos (offset nexttok)

substring :: Text -> Int -> Int -> Text
substring c s e = Text.take (e - s) $ Text.drop s c

match_spaces :: Text -> Maybe (Int, Int)
match_spaces w = Text.findIndex isSpace w >>= \p ->
    case Text.break notSpace (Text.drop p w) of
        (spaces, _) -> Just (p, Text.length spaces + p)
    where notSpace = not . isSpace

-- | Main export of the entire package. Splits a corpus into its constituent
-- sentences.
split_sentences :: Text -> [Text]
split_sentences corpus = map (uncurry $ substring corpus) slices
    where slices = find_breaks corpus

-- | @runPunkt data computation@ runs @computation@ using @data@ collected from
-- a corpus using 'build_punkt_data'.
runPunkt :: PunktData -> Punkt a -> a
runPunkt = flip Reader.runReader
