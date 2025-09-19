#!/usr/bin/env nu

stack --work-dir .stack-work-prof --profile build
open cases/01.in | stack --work-dir .stack-work-prof --profile exec knapsack002-exe -- +RTS -p
