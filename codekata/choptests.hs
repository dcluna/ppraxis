import Chop
import Test.HUnit

test1 = TestCase (assertEqual "chop 1 [1,2,3]," 0 (chop 1 [1,2,3]))
test2 = TestCase (assertEqual "chop 1 [1, 3, 5]," 0 (chop 1 [1, 3, 5]))
test3 = TestCase (assertEqual "chop 7 [1, 3, 5, 7]," 3 (chop 7 [1, 3, 5, 7]))

tests = TestList [TestLabel "test1" test1, TestLabel "test2" test2
                 ,TestLabel "test3" test3]