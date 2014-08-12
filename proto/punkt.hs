{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Parsec as P
import Data.Char (isSpace)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Control.Applicative (Applicative, pure, (<$>), (<*>))
import Control.Monad (ap, liftM)
import Control.Monad.Trans (MonadTrans, lift)
import qualified Text.Regex.PCRE as Regex
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as Char8
import Data.ByteString (ByteString)
import Data.Bits ((.|.))
import Data.Either (either)
import Data.Array ((!))

many_n n p
    | n <= 0 = return []
    | otherwise = (++) <$> P.count n p <*> P.many p

many1_till p end = (:) <$> p <*> P.manyTill p end

sepby_n 0 p sep = P.sepBy p sep
sepby_n n p sep = do
    xs <- (:) <$> p <*> P.count n (sep >> p)
    xss <- P.many $ sep >> p
    return $ xs ++ xss

try_choices = P.choice . map P.try
look_from = P.lookAhead . try_choices

multi_char_punct = try_choices [hyphen, ellipsis, expando_ellipsis]
    where
    hyphen = many_n 2 $ P.char '-'
    ellipsis = many_n 2 $ P.char '.'
    expando_ellipsis = P.char '.' `sepby2` P.space
    sepby2 = sepby_n 2

nonspace = P.satisfy (not . isSpace)
comma = P.char ','

find_all p = fmap catMaybes $ P.many p_or_not
    where p_or_not = fmap Just (P.try p) P.<|> (P.anyChar >> return Nothing)

word_tokenizer = find_all words
    where
    words = try_choices [multi_char_punct, word, fmap (:[]) nonspace]
    word = P.lookAhead word_starter >> nonspace `many1_till` a_wordend
    a_wordend = look_from $ (comma >> look_from wordend') : wordend'
    wordend' = [P.eof, P.space >> return (), nonword_char >> return (),
        multi_char_punct >> return ()]
    word_starter = P.noneOf "\"`:;&#*@-,(){}[]"
    nonword_char = P.oneOf "?!\"';*:@({[]})"

find_end_ctx :: String -> [String]
find_end_ctx xs = map glue $ filter possible_ender pairs
    where
    pairs = zipWith (,) ws $ drop 1 ws where ws = words xs
    possible_ender (w, _) = last (strip_nonword w) `elem` "?!."
    strip_nonword = List.dropWhileEnd (`elem` "?!\"';*:@({[]})")
    glue (word, next) = unwords [word, next]

-- tokens_in :: String -> [Token]
tokens_in xs = (P.runParser word_tokenizer () "" xs)

newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }
    deriving Functor

instance MonadTrans (EitherT a) where
    lift = EitherT . liftM return

instance Monad m => Monad (EitherT a m) where
    return = EitherT . return . return
    maeither >>= f = EitherT $ runEitherT maeither >>= mb f
        where
        mb g (Right stuff) = runEitherT $ g stuff
        mb _ (Left x) = return $ Left x

instance (Monad f, Applicative f) => Applicative (EitherT a f) where
    pure = return
    (<*>) = ap

(~~>) :: (b -> [Either a b]) -> (b -> [Either a b]) -> b -> [Either a b]
(f0 ~~> f1) input = f0 input >>= either (\v -> [Left v]) f1

type TextPos a = (a, Int)
type MatchPos = (Int, Int)

data Entity a = Word a | ParaStart | Ellipsis | Dash | Ordinal
    deriving (Show, Functor)

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
    d <- either ((:[]) . Left) (strip_punct ~~> finalize_tokens) c
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

{-
    \(mtch, pos) -> case Char8.take 1 mtch of
        "." -> [Left $ mktok Ellipsis pos]
        "-" -> [Left $ mktok Dash pos]
        "?" -> [Left $ Token (Word mtch) pos True False]
        "!" -> [Left $ Token (Word mtch) pos True False]
        "\n" -> [Left $ mktok ParaStart pos]
        _ -> []
-}

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
