module FreeListAllocator where

type Address = Int
type Size = Int

data Block = Block {
        bAddr :: Address,
        bSize :: Size
} deriving (Show, Eq)

data AllocatorState = AllocatorState {
        freeBlocks :: [Block]
} deriving (Show, Eq)

initAllocator :: Size -> Maybe AllocatorState
initAllocator maxSize 
                | maxSize <= 0 = Nothing
                | otherwise = Just $ AllocatorState { freeBlocks = [Block 0 maxSize] }

allocate :: Size -> AllocatorState -> Maybe (Maybe Address, AllocatorState)
allocate needSize (AllocatorState blocks) 
        | needSize <= 0 = Nothing
        | otherwise = let (before, found) = break (\b -> bSize b >= needSize) blocks in
                        case found of
                                [] -> Just (Nothing, AllocatorState blocks)
                                (targetBlock : after) -> Just (Just addr, AllocatorState newFreeBlocks) where
                                        addr = bAddr targetBlock
                                        newFreeBlock = Block (addr + needSize) (bSize targetBlock - needSize)
                                        newFreeBlocks = before ++ (if bSize newFreeBlock > 0 then [newFreeBlock] else []) ++ after

deallocate :: Block -> AllocatorState -> AllocatorState
deallocate (Block deAddr deSize) (AllocatorState freeBlocks) =          -- will be rewrite
        let endAddr = deAddr + deSize
            (before, found) = break (\ b -> endAddr <= bAddr b) freeBlocks  --O(n)
        in case found of

                [] ->   if null before
                        then AllocatorState [Block deAddr deSize]
                        else let
                                prevBlock = last before             --O(n)
                                blocksBeforePrev = init before      --O(n)
                                prevAddr = bAddr prevBlock
                                prevSize = bSize prevBlock
                             in if (prevAddr + prevSize) == deAddr
                                --(++) - O(n). Quadruple O(n) in branch
                                then AllocatorState (blocksBeforePrev ++ [Block prevAddr (prevSize + deSize)])
                                else AllocatorState (before ++ [Block deAddr deSize])

                (nextBlock : after) -> let
                        nextAddr = bAddr nextBlock
                        nextSize = bSize nextBlock
                        in if endAddr == nextAddr
                        then if null before
                                then AllocatorState (Block deAddr (deSize + nextSize) : after)
                                else let
                                        prevBlock = last before
                                        blocksBeforePrev = init before
                                        prevAddr = bAddr prevBlock
                                        prevSize = bSize prevBlock
                                     in 
                                        if (prevAddr + prevSize) == deAddr
                                        then AllocatorState (blocksBeforePrev ++ [Block prevAddr (prevSize + deSize + nextSize)] ++ after)
                                        else AllocatorState (before ++ [Block deAddr (deSize + nextSize)] ++ after)

                        else if null before
                                then AllocatorState ([Block deAddr deSize] ++ found)
                                else let 
                                        prevBlock = last before
                                        blocksBeforePrev = init before
                                        prevAddr = bAddr prevBlock
                                        prevSize = bSize prevBlock
                                     in 
                                        if (prevAddr + prevSize) == deAddr
                                        then AllocatorState (blocksBeforePrev ++ [Block prevAddr (prevSize + deSize)] ++ found)
                                        else AllocatorState (before ++ [Block deAddr deSize] ++ found)
