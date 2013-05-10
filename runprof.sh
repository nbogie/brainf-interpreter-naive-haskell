#!/usr/bin/env bash
set -x
set -e
set -u
#This script will run the program many times, producing various 
# profiling graphs (.ps) and reports (.prof) in dir profilings/$1
# After the profiling, it creates the ps with hp2ps.
# See http://www.haskell.org/ghc/docs/latest/html/users_guide/prof-heap.html#retainer-prof
# It also makes a run with a -s report.
#
# It assumes that ./Main has been compiled for profiling
# Author: Neill Bogie

setname=$1
outdir=profilings/$setname
mkdir -p $outdir

input_file=samples/99botles.bf 

./Main $input_file +RTS -s 2> $outdir/s_report.txt

#for flag in hc hr hm hd hy hb; do
for flag in hc hr hd hy; do  #removed hb, hm (biography, module)
  echo processing with flag $flag
  base_outname=$outdir/Main_$flag
  ps_outname=$base_outname.ps
  prof_outname=$base_outname.prof
  rm -f Main.prof
  rm -f Main.hp

  ./Main $input_file +RTS -$flag -p

  set +e
  hp2ps -c -g -e8in Main.hp #only not used in p
  mv Main.ps $ps_outname
  mv Main.prof $prof_outname  #not produced for some heap profilings
  set -e
done

echo "All done.  See results in $outdir"
open $outdir
