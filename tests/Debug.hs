{-# LANGUAGE OverloadedStrings #-}

import NLP.Punkt (Entity, Entity(Word), entity, split_sentences, classify_punkt)
import Data.Text (pack, Text)


split :: FilePath -> IO ()
split file = do
  content <- readFile file
  mapM_ print (split_sentences $ pack content)


f :: Entity Text -> Text -> Bool
f (Word x _) w = x == w
f _ _ = False

classify :: FilePath -> Text -> IO ()
classify file w = do
  content <- readFile file
  mapM_ print $ filter (\n -> f (entity n) w)  (classify_punkt $ pack content)


