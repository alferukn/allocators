module FreeListAllocator
  ( -- * Types
    Address,
    Size,
    Block (..),
    AllocatorState (..),

    -- * Functions
    initAllocator,
    allocate,
    deallocate,
  )
where

-- | Memory offset.
type Address = Int

-- | Memory block size.
type Size = Int

-- | Represents a contiguous segment of memory.
data Block = Block
  { -- | Starting address.
    bAddr :: Address,
    -- | Size of the block.
    bSize :: Size
  }
  deriving (Show, Eq)

-- | Current state of the free memory pool.
data AllocatorState = AllocatorState
  { -- | List of available blocks (ordered by address).
    freeBlocks :: [Block]
  }
  deriving (Show, Eq)

-- | Initializes a new allocator with the given total size.
-- Returns "Nothing" if the size is negative.
initAllocator :: Size -> Maybe AllocatorState
initAllocator maxSize
  | maxSize <= 0 = Nothing
  | otherwise = Just $ AllocatorState {freeBlocks = [Block 0 maxSize]}

-- | Attempts to find and reserve a chunk of memory.
--
-- Returns 'Nothing' if the requested size is invalid (<= 0).
-- Returns 'Just (Nothing, oldState)' if no suitable block is found.
-- Returns 'Just (Just address, newState)' upon successful allocation.
allocate :: Size -> AllocatorState -> Maybe (Maybe Address, AllocatorState)
allocate needSize (AllocatorState blocks)
  | needSize <= 0 = Nothing
  | otherwise =
      let (before, found) = break (\b -> bSize b >= needSize) blocks
       in case found of
            [] -> Just (Nothing, AllocatorState blocks)
            (targetBlock : after) -> Just (Just addr, AllocatorState newFreeBlocks)
              where
                addr = bAddr targetBlock
                newFreeBlock = Block (addr + needSize) (bSize targetBlock - needSize)
                newFreeBlocks = before ++ (if bSize newFreeBlock > 0 then [newFreeBlock] else []) ++ after

-- | Returns a previously allocated block to the pool.
--
-- The function ensures the free list remains sorted and merges the new block
-- with its neighbors if they are adjacent (coalescing).
deallocate :: Block -> AllocatorState -> AllocatorState
deallocate newBlock (AllocatorState blocks) = AllocatorState (insertAndMerge newBlock blocks)
  where
    insertAndMerge :: Block -> [Block] -> [Block]
    insertAndMerge b [] = [b]
    insertAndMerge b1@(Block addr1 size1) (b2@(Block addr2 size2) : rest)
      -- Block is strictly before b2
      | addr1 + size1 < addr2 = b1 : b2 : rest
      -- Block is adjacent to the start of b2: merge and continue
      | addr1 + size1 == addr2 = insertAndMerge (Block addr1 (size1 + size2)) rest
      -- Block is adjacent to the end of b2: merge and continue
      | addr2 + size2 == addr1 = insertAndMerge (Block addr2 (size2 + size1)) rest
      -- Block is strictly after b2: keep b2 and look further
      | addr2 + size2 < addr1 = b2 : insertAndMerge b1 rest
      -- Overlap detected
      | otherwise = error "Memory corruption: overlapping blocks in deallocate"
