#!/usr/bin/env nu

# Format Haskell files
for f in (ls **/*.hs | where name !~ .stack | where name !~ dist-newstyle) {
  stylish-haskell -i $f.name
}
