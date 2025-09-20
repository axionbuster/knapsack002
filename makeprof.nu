#!/usr/bin/env nu

# Build and run the Haskell knapsack002 program with profiling enabled
# in a fresh directory (so it won't overwrite old data).

stack --work-dir .stack-work-prof --profile build

def main [seed_?: int, granularity_?: int] {
 let seed = match $seed_ {
  null => {
   let default_value = 12
   print -e ("using a default seed of " ++ ($default_value | into string))
   $default_value
  }
  _ => $seed_
 }
 let granularity = match $granularity_ {
  null => {
   let default_value = 345
   print -e ("using a default granularity of " ++ ($default_value | into string))
   $default_value
  }
  _ => $granularity_
 }
 # find the number to use. directory scheme: zprof001, zprof002, ...
 let num = (ls | where $it.name =~ '^zprof\d+$' | length) + 1
 let snum = $num | fill --alignment right --character '0' --width 3
 let dirnam = "prof" + $snum
 mkdir $dirnam; cd $dirnam
 stack --work-dir .stack-work-prof --profile exec knapsack002-gen -- $seed $granularity
  | stack --work-dir .stack-work-prof --profile exec knapsack002-exe -- +RTS -p -s -lf -hc
  | tee --stderr { save "knapsack.stats.tmp" } # change extension to avoid checking in
  | save "knapsack.out.tmp"
 cd ..
}
