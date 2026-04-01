module FreeListAllocator where

type Address = Int
type Size = Int

data Block = Block {
        bAddr :: Address,
        bSize :: Size
} deriving (Show, Eq)

data Allocator = Allocator {
        freeBlocks :: [Block]
}

initAllocator :: Size -> Allocator
initAllocator maxSize = Allocator {
        freeBlocks = [Block 0 maxSize]
}

allocate :: Size -> Allocator -> (Maybe Address, Allocator)
allocate needSize (Allocator blocks) = 
        let (before, found) = break (\b -> bSize b >= needSize) blocks in
        case found of
                [] -> (Nothing, Allocator blocks)
                (targetBlock : after) -> (Just addr, Allocator newFreeBlocks) where
                        addr = bAddr targetBlock 
                        newFreeBlock = Block (addr + needSize) (bSize targetBlock - needSize)
                        newFreeBlocks = before ++ (if bSize newFreeBlock > 0 then [newFreeBlock] else []) ++ after