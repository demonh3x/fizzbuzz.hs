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
          applied = foldr (\value acc -> (snd value):acc) [] results
          any = foldr (||) False applied

fizzbuzz :: Int -> String
fizzbuzz = makeFizzBuzz [
    ("Fizz", \n -> mod n 3 == 0),
    ("Buzz", \n -> mod n 5 == 0),
    ("Bang", \n -> mod n 7 == 0)]

tests = TestList [
	TestLabel "Test1"
	(TestCase (assertEqual "Given 1 should return \"1\"" "1" (fizzbuzz 1))),
	TestLabel "Test2"
    (TestCase (assertEqual "Given 2 should return \"2\"" "2" (fizzbuzz 2))),
	TestLabel "Test3"
    (TestCase (assertEqual "Given 3 should return \"Fizz\"" "Fizz" (fizzbuzz 3))),
    TestLabel "Test4"
    (TestCase (assertEqual "Given 4 should return \"4\"" "4" (fizzbuzz 4))),
    TestLabel "Test5"
    (TestCase (assertEqual "Given 5 should return \"Buzz\"" "Buzz" (fizzbuzz 5))),
    TestLabel "Test6"
    (TestCase (assertEqual "Given 6 should return \"Fizz\"" "Fizz" (fizzbuzz 6))),
    TestLabel "Test7"
    (TestCase (assertEqual "Given 7 should return \"Bang\"" "Bang" (fizzbuzz 7))),
    TestLabel "Test10"
    (TestCase (assertEqual "Given 10 should return \"Buzz\"" "Buzz" (fizzbuzz 10))),
    TestLabel "Test15"
    (TestCase (assertEqual "Given 15 should return \"FizzBuzz\"" "FizzBuzz" (fizzbuzz 15)))]

main = do runTestTT tests
