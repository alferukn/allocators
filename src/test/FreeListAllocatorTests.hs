import Test.Tasty
import Test.Tasty.HUnit
import FreeListAllocator

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
        [ unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Test"
        [ testCase "first test" $ do
        let initialState = initAllocator 100
            (maybeAddr, newState) = allocate 40 initialState
        maybeAddr @?= Just 0
        freeBlocks newState @?= [Block 40 60]
        ]