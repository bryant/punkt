{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isUpper, toUpper, toLower)
import Data.Maybe (catMaybes)
import qualified Text.Regex.PCRE as Regex
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Bits ((.|.))
import Data.Either (either)
import Data.Array ((!))

(~~>) :: (b -> [Either a b]) -> (b -> [Either a b]) -> b -> [Either a b]
(f0 ~~> f1) input = f0 input >>= either (\v -> [Left v]) f1

type TextPos a = (a, Int)
type MatchPos = (Int, Int)

data Entity a = Word a | ParaStart | Ellipsis | Dash | Ordinal
    deriving (Show, Eq, Functor)

data Token a = Token
    { entity :: Entity a
    , position :: MatchPos
    , sentend :: Bool
    , abbrev :: Bool
    }
    deriving Show

type Chunks a = [Either (Token a) (TextPos a)]

-- process :: ByteString -> Chunks Text
process text = do
    -- fmap decode_utf8 ((mark_multi ~~> sep_words) (text, 0))
    b <- (mark_multi ~~> sep_words) (encodeUtf8 text, 0)
    let c = decode_utf8 b
    d <- either ((:[]) . Left) (strip_punct ~~> split_contract ~~> finalize_tokens) c
    return d

mktok e pos = Token e pos False False

mark_multi :: TextPos ByteString -> Chunks ByteString
mark_multi corpus = extract re corpus tokenify
    where
    tokenify (mtch, pos)
        | c == "." || mtch == encodeUtf8 "…" = [Left $ mktok Ellipsis pos]
        | c == "-" || mtch == encodeUtf8 "—" = [Left $ mktok Dash pos]
        | c == "?" || c == "!" = [Left $ Token (Word mtch) pos True False]
        | c == "\n" = [Left $ mktok ParaStart pos]
        | otherwise = []
        where c = Char8.take 1 mtch
    re = compile_re "(?:-{2,}|—|\\.{2,}|\\.(?: \\.){1,}|…|[!\\?]{1,}|\n{2,})"

sep_words :: TextPos ByteString -> Chunks ByteString
sep_words (chunk, pos) = mark pos $ Char8.words chunk
    where
    mark _ [] = []
    mark n (x:xs) = Right (x, n) : mark (n + Char8.length x) xs

decode_utf8 (Right (t, pos)) = Right (decodeUtf8 t, pos)
decode_utf8 (Left t@(Token {entity=e})) =
    Left $ t { entity = fmap decodeUtf8 e }

strip_punct :: TextPos Text -> Chunks Text
strip_punct (chunk, pos) = [Right (strip chunk, pos)]
    where strip = Text.dropAround (`elem` ",;:()[]{}“”’\"\')")

split_contract :: TextPos Text -> Chunks Text
split_contract (chunk, pos) = rv 0 $ Text.split (`elem` "'’") chunk
    where
    rv _ [] = []
    rv n (x:xs) = Right (x, n) : rv (n + Text.length x) xs

finalize_tokens (chunk, pos) =
    [Left $ mktok (Word chunk) (pos, Text.length chunk)]

extract :: Regex.Regex -> TextPos ByteString
        -> ((ByteString, MatchPos) -> Chunks ByteString)
        -> Chunks ByteString
extract re (corpus, comp) whenmatch = concat $ berk matches corpus 0
    where
    -- cs:  abcdefghijklmnopqrs...
    -- ps: [  ^--  ^---   ^-   ...]
    -- =>  [ab, cde, fg, hijk, lmn, op, qrs...]
    berk [] cs base = [[Right (cs, base + comp)]]
    berk ((off, len):ps) cs base = if pre == "" then rv else [Right (pre, base + comp)] : rv
        where
        (pre, chunk) = Char8.splitAt (off - base) cs
        (m, post) = Char8.splitAt len chunk
        rv = whenmatch (m, (off, len)) : berk ps post (off + len)
    matches = map (! 0) $ Regex.matchAll re corpus

dll a b ab n = -2 * (null - alt)
    where
    (a', b', ab', n') = (fromIntegral a, fromIntegral b, fromIntegral ab, fromIntegral n)
    null = ab' * log p1 + (a' - ab') * log (1 - p1)
    alt = ab' * log p2 + (a' - ab') * log (1 - p2)
    (p1, p2) = (b' / n', 0.99)
    log = logBase 10

word_tokens toks = catMaybes $ map words_of toks
    where
    words_of (Word w) = Just w
    words_of _ = Nothing

filter_with f = filter f . word_tokens . map entity

num_words = fromIntegral . length . filter_with (const True)

num_periods = fromIntegral . length . filter_with (\w -> Text.last w == '.')

occurs werd = fromIntegral . length . filter_with (\w -> Text.toCaseFold w == Text.toCaseFold werd)

occurs_in :: Text -> [Token Text] -> Bool
occurs_in word toks = any satisfying toks
    where
    satisfying (Token {entity=(Word w)}) = w == word
    satisfying _ = False

occurs_after :: (Token Text -> Bool) -> Text -> [Token Text] -> Bool
occurs_after f word toks = any satisfying tokpairs
    where
    satisfying (first, Token {entity=(Word w)}) = w == word && f first
    satisfying _ = False
    tokpairs = zipWith (,) toks $ drop 1 toks

prob_abbrev w toks = ll_abbr * f_len * f_periods * f_penalty
    where
    w_period = w `Text.snoc` '.'
    ll_abbr = dll (occurs w_period toks + occurs w toks) (num_periods toks) (occurs w_period toks) (num_words toks)
    len = fromIntegral . Text.length $ Text.filter (/= '.') w
    f_len = 1 / exp len
    f_periods = fromIntegral $ Text.length (Text.filter (== '.') w) + 1
    f_penalty = 1 / len ^ occurs w toks

compile_re :: String -> Regex.Regex
compile_re = Regex.makeRegexOpts opts execopts
    where
    opts = Regex.compUTF8 .|. Regex.compDotAll
    execopts = Regex.execBlank

classify_periods toks = map maybe_abbrev toks
    where
    maybe_abbrev t@(Token {entity=(Word w)})
        | Text.last w == '.' = case prob_abbrev (Text.init w) toks >= 0.3 of
            False -> t { sentend = True }
            True -> t { abbrev = True }
    maybe_abbrev t = t

-- given "tok0 tok1", where tok0 is abbr, decide if it is also a sentence
-- end through the casing of tok1.
-- > ortho_heuristic tok1 toks = Just True -> tok0 is also an end
--                               Just False -> tok0 is definitively not
--                               Nothing -> undecided
ortho_heuristic :: Text -> [Token Text] -> Maybe Bool
ortho_heuristic w toks = case isUpper $ Text.head w of
    False -> case occurs_in titlecased toks || occurs_after sentend w toks of
        False -> Nothing
        True -> Just False
    True -> case occurs_in lowercased toks && not (occurs_after non_ender w toks) of
        False -> Nothing
        True -> Just True
    where
    non_ender t = not $ sentend t || entity t == ParaStart
    titlecased = toUpper (Text.head w) `Text.cons` Text.tail w
    lowercased = toLower (Text.head w) `Text.cons` Text.tail w
