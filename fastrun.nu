#!/usr/bin/env nu

print "build main"
stack build

print "build prof"
stack build --work-dir .stack-work-prof --profile

let path = (stack path --dist-dir)
def makepath [p: string, base_?: string] {
 let base = match $base_ {
  null => $path
  _ => $base_
 }
 $base | path join build $p $p
}
let exe = (makepath knapsack002-exe)
let gen = (makepath knapsack002-gen)
let out = ^$gen 12 345 | collect

print "pre-run"
let rt1 = $out | timeit { $exe out> /dev/null }
$rt1 | print

print "run (no profile)"
let rt2 = $out | timeit { $exe out> /dev/null }
$rt2 | print

print "pre-run (with profile)"
let exe2 = (makepath knapsack002-exe (stack path --work-dir .stack-work-prof --dist-dir)) | collect
let rt3 = $out | timeit { $exe2 out> /dev/null }
$rt3 | print

print "run (with profile)"
let exe2 = (makepath knapsack002-exe (stack path --work-dir .stack-work-prof --dist-dir)) | collect
let rt4 = $out | timeit { $exe2 out> /dev/null }
$rt4 | print

print "done"
[$rt2, $rt4]
