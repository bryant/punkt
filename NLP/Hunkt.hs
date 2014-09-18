{-# LANGUAGE OverloadedStrings #-}

module NLP.Hunkt where

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Map (Map)
import Data.Char (isLower)
import qualified Data.Map as Map
import qualified Data.List as List

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

-- probability that (w_ `snoc` '.') is an abbreviation.
prob_abbr :: Map Text Int -> Int -> Int -> Text -> Double
prob_abbr ctr nperiods ntoks w_ = log_like * f_len * f_periods * f_penalty
    where
    log_like = strunk_log (count wp + count w) (fromIntegral nperiods)
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
prob_starter abbrctr orthoctr nenders ntoks w_ =
    dunning_log (fromIntegral nenders) (count w + count wp) count_w_start
                (fromIntegral ntoks)
    where
    w = norm w_
    wp = w `Text.snoc` '.'

    count w = fromIntegral $ Map.findWithDefault 0 w abbrctr
    count_w_start = fromIntegral $ freq_after_ender orthofreq
    orthofreq = Map.findWithDefault (OrthoFreq 0 0 0 0 0) w orthoctr
