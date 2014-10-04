import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.QuickCheck (
      forAll
    , choose
    , NonNegative(..)
    , Positive(..)
    , Arbitrary(..)
    , testProperty
    )
import NLP.Punkt (dunning_log)
import KnownCount (known_count_tests)
import Brown (benchmark_brown)

main = defaultMain $ testGroup "Tests"
    [ dunning_equiv
    , known_count_tests
    , benchmark_brown
    ]

ellr :: Int -> Int -> Int -> Int -> Double
ellr purea pureb ab neither =
    -2 * totes * (entropy all - entropy rows - entropy cols)
    where
    totes = fromIntegral $ sum all
    all = [purea, pureb, ab, neither]
    rows = [ab + pureb, purea + neither]
    cols = [ab + purea, pureb + neither]

    entropy :: [Int] -> Double
    entropy fs = - (sum $ map (ent . (/ n) . fromIntegral) fs)
        where
        n = fromIntegral $ sum fs
        ent 0 = 0  -- lim_{x -> 0} {x * log x} = 0
        ent p = p * log p

(~~) :: (Ord f, Floating f) => f -> f -> Bool
x ~~ y = abs (x - y) < epsilon where epsilon = 10 ** (-12)

data EventSet = EventSet Int Int Int Int deriving Show

instance Arbitrary EventSet where
    arbitrary = do
        NonNegative purea <- arbitrary
        NonNegative pureb <- arbitrary
        ab <- choose (1, min purea pureb)
        Positive neither <- arbitrary
        return $ EventSet purea pureb ab neither

dunning_equiv :: TestTree
dunning_equiv = testProperty "Dunning LLR-entropy equivalence" $
    forAll arbitrary llr_equiv
    where
    llr_equiv (EventSet pa pb ab neither) =
        ellr pa pb ab neither ~~ dunning_log a b ab' n
        where [a, b, ab', n] = map fromIntegral [ pa + ab, pb + ab, ab
                                                , pa + pb + ab + neither]
