% THREADPOOL_TIMINGS   Just tests the speedup of different approaches to
% multithreading.  Fun fact: vectorized MATLAB code is the best way to do
% absolutely everything if you can figure out how!
% 
% In order to use this file, you first have to compile the MEX files / 
% functions properly:
%
%   mex -R2018a madd.c
%   mex -R2018a madd_threadpool.c threadpool.c

clear all; %#ok<*CLALL>
nthreads = maxNumCompThreads(1);
a = rand(1000); 
disp([newline, 'Single-threaded:']);
tic; 
for i = 1:1000
    b = madd(a, a);
end
toc;
maxNumCompThreads(nthreads);
disp('---');

clear all;
a = rand(1000); 
disp('Multi-threaded, no threadpool:');
tic; 
for i = 1:1000
    b = madd(a, a);
end
toc;
disp('---');

clear all;
disp('Multi-threaded, threadpool:');
a = rand(1000); 
tic; 
for i = 1:1000
    b = madd_threadpool(a, a);
end
toc;
disp('---');

clear all;
disp('Multi-threaded, vectorized:');
a = rand(1000); 
tic;
for i = 1:1000
    b = a + a;
end
toc;
disp(' ');