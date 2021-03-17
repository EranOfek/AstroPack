function [Dark,A]=worker_comm(A)
%
% PoolObj=parpool
% F=parfeval(@imUtil.tests.worker_comm,2,5)
% delete(PoolObj)



Dark = imUtil.util.file.load_check('Dark.mat');
Dark


