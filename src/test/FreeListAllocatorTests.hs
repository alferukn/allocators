import Test.Tasty
import Test.Tasty.HUnit
import FreeListAllocator

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests"
        [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Test"
        [
        testCase "Init: valid size" $ do
        let state = initAllocator 100
        state @?= Just (AllocatorState [Block 0 100])

        , testCase "Init: zero or negative size" $ do
        let state1 = initAllocator 0
            state2 = initAllocator (-100)
        state1 @?= Nothing
        state2 @?= Nothing

        , testCase "Successful allocation" $ do
        let initState = initAllocator 100
            (addr1, state1) = allocate 20 initState
            (addr2, state2) = allocate 80 state1
        addr1 @?= Just 0
        addr2 @?= Just 20
        state1 @?= AllocatorState [Block 20 80]
        state2 @?= AllocatorState []

        , testCase "Unsuccessful allocation" $ do
        let initState = initAllocator 100
            (addr1, state1) = allocate 80 initState
            (addr2, state2) = allocate 80 state1
        addr1 @?= Just 0
        addr2 @?= Nothing
        state1 @?= AllocatorState [Block 80 20]
        state2 @?= AllocatorState [Block 80 20]

        , testCase "Allocate: zero or negative size" $ do
        let initState = initAllocator 100
            (addr1, state1) = allocate 0 initState
            (addr2, state2) = allocate (-5) state1
        addr1 @?= Nothing
        addr2 @?= Nothing
        state1 @?= AllocatorState [Block 0 100]
        state2 @?= AllocatorState [Block 0 100]

        , testCase "Allocate: fit in the middle of free list" $ do
        let state = AllocatorState [Block 0 10, Block 20 20, Block 50 100]
            (addr, newState) = allocate 20 state
        addr @?= Just 20
        newState @?= AllocatorState [Block 0 10, Block 50 100]

        , testCase "Basic deallocate" $ do
        let startState = AllocatorState []
            state1 = deallocate (Block 30 10) startState
        state1 @?= AllocatorState [Block 30 10]

        , testCase "Deallocate: new block is glued in front of the block" $ do
        let startState = AllocatorState [Block 50 20]
            state1 = deallocate (Block 30 20) startState
        state1 @?= AllocatorState [Block 30 40]


        , testCase "Deallocate: new block is glued after the block" $ do
        let startState = AllocatorState [Block 50 20]
            state1 = deallocate (Block 70 20) startState
        state1 @?= AllocatorState [Block 50 40]


        , testCase "Deallocate: new block is glued between the blocks" $ do
        let startState = AllocatorState [Block 20 20, Block 60 10]
            state1 = deallocate (Block 40 20) startState
        state1 @?= AllocatorState [Block 20 50]

        , testCase "Deallocate: glue to the very first block" $ do
        let startState = AllocatorState [Block 0 10, Block 50 10]
            state1 = deallocate (Block 10 10) startState
        state1 @?= AllocatorState [Block 0 20, Block 50 10]

        , testCase "Deallocate: glue to the very last block" $ do
        let startState = AllocatorState [Block 0 10, Block 50 10]
            state1 = deallocate (Block 40 10) startState
        state1 @?= AllocatorState [Block 0 10, Block 40 20]

        , testCase "Deallocate: complex fragmented state" $ do
        let startState = AllocatorState [Block 0 10, Block 40 10, Block 80 10]
            state1 = deallocate (Block 10 20) startState
        state1 @?= AllocatorState [Block 0 30, Block 40 10, Block 80 10]

        , testCase "Deallocate: insert without gluing" $ do
        let startState = AllocatorState [Block 10 10, Block 100 10]
            state1 = deallocate (Block 50 10) startState
        state1 @?= AllocatorState [Block 10 10, Block 50 10, Block 100 10]

        , testCase "Deallocate: maintain address order" $ do
        let startState = AllocatorState [Block 10 10, Block 100 10]
            state1 = deallocate (Block 40 10) startState
            state2 = deallocate (Block 70 10) state1
        let addrs = map bAddr (freeBlocks state2)
        addrs @?= [10, 40, 70, 100]
        ]
