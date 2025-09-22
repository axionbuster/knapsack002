#!/usr/bin/env nu

# Build with LLVM 14
source llvm-14.nu
stack build --ghc-options="-fllvm"
