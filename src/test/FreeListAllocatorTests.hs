import FreeListAllocator
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All Tests"
    [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ initTests,
      allocateTests,
      deallocateTests
    ]

initTests :: TestTree
initTests =
  testGroup
    "Initialization"
    [ testCase "Init: valid size" $ do
        let state = initAllocator 100
        state @?= Just (AllocatorState [Block 0 100]),
      --
      testCase "Init: zero or negative size" $ do
        let state1 = initAllocator 0
            state2 = initAllocator (-100)
        state1 @?= Nothing
        state2 @?= Nothing
    ]

allocateTests :: TestTree
allocateTests =
  testGroup
    "Alloccation"
    [ testCase "Successful allocation" $ do
        let nState = initAllocator 100
        case nState of
          Nothing -> assertFailure "initAllocator failed to create state"
          Just initState -> do
            let res1 = allocate 20 initState
            res1 @?= Just (Just 0, AllocatorState [Block 20 80])

            case res1 of
              Just (Just _, state1) -> do
                let res2 = allocate 80 state1
                res2 @?= Just (Just 20, AllocatorState [])
              _ -> assertFailure "First allocation unexpectedly failed",
      --
      testCase "Unsuccessful allocation" $ do
        let nState = initAllocator 100
        case nState of
          Nothing -> assertFailure "initAllocator failed to create state"
          Just initState -> do
            let res1 = allocate 80 initState
            res1 @?= Just (Just 0, AllocatorState [Block 80 20])

            case res1 of
              Just (Just _, state1) -> do
                let res2 = allocate 80 state1
                res2 @?= Just (Nothing, AllocatorState [Block 80 20])
              _ -> assertFailure "First allocation unexpectedly failed",
      --
      testCase "Allocate: zero or negative size" $ do
        let nState = initAllocator 100
        case nState of
          Nothing -> assertFailure "initAllocator failed to create state"
          Just initState -> do
            let res1 = allocate 0 initState
                res2 = allocate (-5) initState
            res1 @?= Nothing
            res2 @?= Nothing,
      --
      testCase "Allocate: fit in the middle of free list" $ do
        let state = AllocatorState [Block 0 10, Block 20 20, Block 50 100]
            res = allocate 20 state
        res
          @?= Just (Just 20, AllocatorState [Block 0 10, Block 50 100])
    ]

deallocateTests :: TestTree
deallocateTests =
  testGroup
    "Deallocation"
    [ testCase "Basic deallocate" $ do
        let startState = AllocatorState []
            state1 = deallocate (Block 30 10) startState
        state1 @?= AllocatorState [Block 30 10],
      --
      testCase
        "Deallocate: new block is glued in front of the block"
        $ do
          let startState = AllocatorState [Block 50 20]
              state1 = deallocate (Block 30 20) startState
          state1 @?= AllocatorState [Block 30 40],
      --
      testCase "Deallocate: new block is glued after the block" $ do
        let startState = AllocatorState [Block 50 20]
            state1 = deallocate (Block 70 20) startState
        state1 @?= AllocatorState [Block 50 40],
      --
      testCase "Deallocate: new block is glued between the blocks" $ do
        let startState = AllocatorState [Block 20 20, Block 60 10]
            state1 = deallocate (Block 40 20) startState
        state1 @?= AllocatorState [Block 20 50],
      --
      testCase "Deallocate: glue to the very first block" $ do
        let startState = AllocatorState [Block 0 10, Block 50 10]
            state1 = deallocate (Block 10 10) startState
        state1 @?= AllocatorState [Block 0 20, Block 50 10],
      --
      testCase "Deallocate: glue to the very last block" $ do
        let startState = AllocatorState [Block 0 10, Block 50 10]
            state1 = deallocate (Block 40 10) startState
        state1 @?= AllocatorState [Block 0 10, Block 40 20],
      --
      testCase "Deallocate: complex fragmented state" $ do
        let startState = AllocatorState [Block 0 10, Block 40 10, Block 80 10]
            state1 = deallocate (Block 10 20) startState
        state1
          @?= AllocatorState [Block 0 30, Block 40 10, Block 80 10],
      --
      testCase "Deallocate: insert without gluing" $ do
        let startState = AllocatorState [Block 10 10, Block 100 10]
            state1 = deallocate (Block 50 10) startState
        state1
          @?= AllocatorState [Block 10 10, Block 50 10, Block 100 10],
      --
      testCase "Deallocate: maintain address order" $ do
        let startState = AllocatorState [Block 10 10, Block 100 10]
            state1 = deallocate (Block 40 10) startState
            state2 = deallocate (Block 70 10) state1
        let addrs = map bAddr (freeBlocks state2)
        addrs @?= [10, 40, 70, 100]
    ]
