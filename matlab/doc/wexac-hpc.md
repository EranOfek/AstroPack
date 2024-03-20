# MATLAB WEXAC - WIS HPC


ssh -X __@access4.wexac.weizmann.ac.il


module load matlab/R2023a



### Sync computer folder into wexac

rsync -va  ./Matlab __@access4.wexac.weizmann.ac.il:/home/labs/...



### Start 

bsub -q physics-short/medium/long  -n _ -eo err%J.txt -oo out%J.txt -R "span[hosts=1] rusage[mem=_]"  "matlab -nodisplay < /home/labs/...n/_.m"



## Modules

https://www.mathworks.com/matlabcentral/answers/93913-how-can-i-determine-what-add-ons-i-have-installed


