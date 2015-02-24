import Test.HUnit

fizzbuzz :: Int -> String
fizzbuzz 3 = "Fizz"
fizzbuzz 5 = "Buzz"
fizzbuzz x = show x

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
    (TestCase (assertEqual "Given 5 should return \"Fizz\"" "Fizz" (fizzbuzz 6)))]

main = do runTestTT tests
