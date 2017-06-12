#!/bin/bash

outfolder="/home/rstudio/JASA/output/temp"
rscript="base_forecasts.R"

task=$1
njobs=$2
nperjobs=$3

if [[ "$task" = "aggregate" ]]; then
  doagg=TRUE
  algo="DETS"
elif [[ "$task" = "bottom" ]]; then
  doagg=FALSE
  algo="KD-IC-NML"
else
	echo "Task must be aggregate or bottom" 1>&2
	exit 1
fi

allijobs=$(seq 1 $njobs )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1)* ($nperjobs) + 1 ))
  end=$(( 0 + ($ijob - 1)* ($nperjobs) + ($nperjobs) ))
  alliseries=( $(seq $start $end ) )
  
  echo "${alliseries[@]}"
  
  Rscript --vanilla $rscript $algo $doagg ${alliseries[@]} > "$outfolder/base_forecasts-$doagg-$algo-$ijob.Rout" 2> "$outfolder/base_forecasts-$doagg-$algo-$ijob.err" &

done



