#!/usr/bin/env nu

def main [--use-llvm] {
 def warning [] {
  print -e "Before switching code generators, clean the project:"
  print -e " stack clean"
 }

 let flags: string = if $use_llvm {
  source ./llvm-14.nu
  print -e "Using LLVM 14"
  print "Using LLVM 14"
  warning
  "--ghc-options=-fllvm"
 } else {
  print -e "Using the native code generator"
  print "Using the native code generator"
  warning
  ""
 }

 print "build main"
 stack build $flags

 print "build prof"
 stack build $flags --work-dir .stack-work-prof --profile

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

 let exeP = (makepath knapsack002-exe (stack path --work-dir .stack-work-prof --dist-dir))

 print "pre-run (with profile)"
 let rt3 = $out | timeit { $exeP out> /dev/null }
 $rt3 | print

 print "run (with profile)"
 let rt4 = $out | timeit { $exeP out> /dev/null }
 $rt4 | print

 print "done"
 [$rt2, $rt4]
}
