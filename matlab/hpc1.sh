#!/bin/bash

# Submit job
bsub -J "pipeline1", -oo "output.txt" -eo "error.txt" -n 16 -R "rusage[mem=128000]" -W 01:00 "module load matlab/R2023a;  matlab -nodisplay -r \"pipeline.DemonLAST.unitTest;exit"\"

sleep 60

tail -f output.txt

