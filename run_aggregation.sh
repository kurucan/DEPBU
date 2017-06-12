#!/bin/bash

outfolder="/home/rstudio/JASA/output/temp"
rscript="aggregation.R"

njobs=12
nperjobs=368

allijobs=$(seq 1 $njobs )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1) * $nperjobs + 1 ))
  end=$(( 0 + ($ijob - 1) * $nperjobs + $nperjobs ))
  allidtest=( $(seq $start $end ) )
  
  echo "${allidtest[@]}"
  Rscript --vanilla $rscript $ijob ${allidtest[@]} > "$outfolder/aggregation-$ijob.Rout" 2> "$outfolder/aggregation-$ijob.err" &
done
