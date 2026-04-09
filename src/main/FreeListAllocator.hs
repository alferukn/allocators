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
deallocate newBlock (AllocatorState blocks) = AllocatorState (insertAndMerge newBlock blocks)
        where
                insertAndMerge :: Block -> [Block] -> [Block]

                insertAndMerge b [] = [b]

                insertAndMerge b1@(Block addr1 size1) (b2@(Block addr2 size2) : rest)
                        | addr1 + size1 < addr2  = b1 : b2 : rest
                        | addr1 + size1 == addr2 = insertAndMerge (Block addr1 (size1 + size2)) rest
                        | addr2 + size2 == addr1 = insertAndMerge (Block addr2 (size2 + size1)) rest
                        | addr2 + size2 < addr1  = b2 : insertAndMerge b1 rest
                        | otherwise = error "Memory corruption: overlapping blocks in deallocate"
