PWD = pwd;
cd ~/matlab/AstroPack/matlab/startup/
startup
cd(PWD)

addpath('~/matlab/data/');
addpath('~/matlab/data/spec/');
addpath('~/matlab/data/spec/SpecGalQSO/');
addpath('~/matlab/data/spec/PicklesStellarSpec/');

VO.prep.prep_data_dir;

%addpath('~/matlab/catsHTM/GAIA/DR2_19/');
%addpath('~/matlab/catsHTM/GALEX/DR6Plus7/');