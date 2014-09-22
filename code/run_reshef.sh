for t in {1..11}
do
  for n in {1..30}
  do
    # qsub -S /bin/bash -l h_vmem=2G -cwd -N reshef_$t_$n \
    # /is/ei/dlopez/R-2.14.1/bin/R -f run_reshef.r --args $n $t
    R -f run_reshef.r --args $n $t
  done
done
