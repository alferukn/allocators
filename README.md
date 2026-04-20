# Free List Allocator

![CI](https://github.com/alferukn/allocators/actions/workflows/ci.yaml/badge.svg)
![HLint](https://img.shields.io/badge/style-HLint-blueviolet?logo=haskell&logoColor=white)
![Formatter](https://img.shields.io/badge/format-Ormolu-orange?logo=haskell&logoColor=white)
![Cabal](https://img.shields.io/badge/Cabal-3.0+-blue?logo=haskell&logoColor=white)
![GHC](https://img.shields.io/badge/GHC-9.6.7+-5e5086?logo=haskell&logoColor=white)
![License](https://img.shields.io/badge/license-MIT-lightgrey)

## Overview

This repository contains a memory allocator implemented in **Haskell** using the Free-List approach. 

The primary goal of this project is to simulate manual memory management in a purely functional language. It allows users to initialize a block of memory, allocate specific chunks, and deallocate them safely, while the system automatically handles the fragmentation and merging of available space.

## Technical Details & Architecture

The project implements a purely functional memory management system. Unlike imperative allocators, every operation here is deterministic and returns a new state of the entire system.

### Data Structures
* **Block**: A simple record storing `Address` and `Size`. It represents a "hole" in the memory that can be filled.
* **AllocatorState**: A wrapper around `[Block]`. The list is strictly maintained as an **ordered sequence** of non-adjacent free spaces.

### Algorithms & Complexity
1. **Allocation**: 
   * **Logic**: The allocator scans the list from the beginning and picks the first block where `bSize >= requested`. 
   * **Complexity**: $O(n)$ in the worst case, where $n$ is the number of free blocks.
2. **Deallocation (Coalescing)**:
   * **Logic**: When a block is freed, the system doesn't just add it to the list. It checks if the new block touches any existing free blocks. If it does, they are merged into one.
   * **Invariants**: This ensures that the free list is always minimal and contains the largest possible contiguous chunks.
   * **Complexity**: $O(n)$ to find the correct insertion point and perform merges.

---

## Quick Start

### Requirements
* `GHC` **9.6.7+**
* `Cabal` **3.0+**

### 1. Build
```bash
cabal build
```
### 2. Test
```bash
cabal test
```
### Usage Example
```haskell
import FreeListAllocator

main :: IO ()
main = do
    -- 1. Initialize allocator with 100 units of memory
    let (Just state0) = initAllocator 100
    print state0 -- AllocatorState {freeBlocks = [Block {bAddr = 0, bSize = 100}]}

    -- 2. Allocate 20 units
    let Just (Just addr, state1) = allocate 20 state0
    putStrLn $ "Allocated at: " ++ show addr -- Allocated at: 0

    -- 3. Deallocate the block back
    let state2 = deallocate (Block addr 20) state1
    print state2 -- Back to initial state (after merging)
```

---

## API Reference

The module `FreeListAllocator` exports the following interface:

### Types
* `Address`: Type alias for `Int`. Represents the start offset of a memory block.
* `Size`: Type alias for `Int`. Represents the length of a memory block.
* `Block`: A record containing `bAddr :: Address` and `bSize :: Size`.
* `AllocatorState`: Represents the current state of free memory.

### Functions
| Function | Signature | Description |
|:---|:---|:---|
| **initAllocator** | `Size -> Maybe AllocatorState` | Creates a new pool. Returns `Nothing` if size is non-positive. |
| **allocate** | `Size -> AllocatorState -> Maybe (Maybe Address, AllocatorState)` | Attempts to find space. Returns `Just (Just addr, newState)` on success, `Just (Nothing, oldState)` if no space is found, or `Nothing` on invalid input size. |
| **deallocate** | `Block -> AllocatorState -> AllocatorState` | Returns a block to the pool and performs coalescing. |

---

## Project Structure
```text
/src
├── main
│   └── FreeListAllocator.hs
└── test
    └── FreeListAllocatorTests.hs
```
