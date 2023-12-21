# Parallel File-System Statistics Utility
Traverse the file system, collect various statistics about each file and directory in a parallel fashion, and output them in a visually-appealing utility.

Inspiration taken from a Rust implementation:
https://github.com/KSXGitHub/parallel-disk-usage

Requires the following additional packages:
- async (Control.Concurrent.Async, https://hackage.haskell.org/package/async)

# Usage
./main (traversal-type) (absolute-path-root-directory)

Traversal Types:

0 - sequential traversal

1 - parallel traversal, version 1

2 - parallel traversal, version 2


# Build
stack build

stack ghc -- -O2 -threaded -rtsopts -eventlog app/Main.hs

time ./app/Main (0,1, or 2) (rootDir absolute path)

# Reproducing Test Results
All of my tests were ran on a directory containing a fresh clone of the linux kernel and
the source from this repo.

Example
.
\_ linux
\_ parallel-fs-stats

# Test Environment
Ubuntu VM, 4 vCPu, 12 GB Memory, 100 GB Disk

2018 Intel MacbookPro

