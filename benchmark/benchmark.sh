#!/usr/bin/bash
echo 'python version'
time ./benchmark.py

echo '---------------------------'
echo 'haskell version interpreted'
time ./benchmark.hs

echo '---------------------------'
ghc -O2 benchmark.hs
echo 'haskell version compiled'
time ./benchmark
