punkt
=====

Multilingual unsupervised sentence tokenization with Punkt.

## Usage

Note that abbreviations are detected at run time without the aid of a pre-built
abbreviation list:

```haskell
import Data.Text (Text, pack)
import NLP.Punkt (split_sentences)

corpus :: Text
corpus = pack "Look, Ma! The quick brown Mr. T. rex swallowed the lazy dog. \
              \It really did!"

main :: IO ()
main = mapM_ print (split_sentences corpus)
```

yields:

```
"Look, Ma!"
"The quick brown Mr. T. rex swallowed the lazy dog."
"It really did!"
```

## References

Kiss, Tibor, and Jan Strunk. "Unsupervised multilingual sentence boundary
detection." Computational Linguistics 32.4 (2006): 485-525.

## TODO

- parallelize
- modularize tokenization
  - custom tokenization rules
- needs to go fasterer
