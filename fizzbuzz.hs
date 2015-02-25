import Test.HUnit

makeFizzBuzz :: [(String, Int -> Bool)] -> Int -> String
makeFizzBuzz factors n
    | any = foldl
                   (\acc value ->
                       if (snd value)
                       then acc ++ (fst value)
                       else acc)
                   ""
                   results
    | not (any) = show n
    where results = map (\val -> ((fst val), (snd val) n)) factors
          any = foldr (||) False (map snd results)

dividableBy :: Int -> Int -> Bool
dividableBy n x = mod x n == 0

fizzbuzz :: Int -> String
fizzbuzz = makeFizzBuzz
    [("Fizz", dividableBy 3)
    ,("Buzz", dividableBy 5)
    ,("Bang", dividableBy 7)
    ]

tests = TestList (map
    (\c ->
        TestLabel ("Test" ++ show (fst c))
        (TestCase (assertEqual
            ("Given " ++ show (fst c) ++ " should return \"" ++ (snd c) ++ "\"")
            (snd c)
            (fizzbuzz (fst c)))))
    [(1, "1")
    ,(2, "2")
    ,(3, "Fizz")
    ,(4, "4")
    ,(5, "Buzz")
    ,(6, "Fizz")
    ,(7, "Bang")
    ,(10, "Buzz")
    ,(15, "FizzBuzz")
    ,(105, "FizzBuzzBang")
    ])

main = do runTestTT tests
