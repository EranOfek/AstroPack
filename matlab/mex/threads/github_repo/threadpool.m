% THREADPOOL   Persistent thread pool for MATLAB MEX files so that you
% don't have to suffer the overhead of creating / destroying threads
% every time you run a MEX file.  Can significantly speed up execution
% of MEX files, especially if you're solving a lot of smaller problems
% in a loop, or calling into the MEX file a lot in a loop.
% 
%   To use this in your MEX file, you need to:
%      #include "threadpool.c"
% 
%   From there, the only two commands you need to know are:
%      - AddThreadPoolJob(void (*jobfunc)(void*), void* jobargs);
%      - SynchronizeThreads();
% 
%   AddThreadPoolJob should be used in place of creating HANDLEs or
%   threads or whatever fun stuff is needed to create a standalone multi-
%   threaded MEX file. The first argument that needs to be passed is an 
%   address to the function that you'll be running on your data.  The 
%   function signature must be:
% 
%      void jobfunc(void* jobargs)
% 
%   ... but with jobfunc or jobargs called whatever you want.
%   The second argument has to be a pointer to an array or a struct that 
%   can be cast to a void poitner (void*), and that allows you to access 
%   the data that needs to be manipulated.  Since the function can't  
%   return anything, this should include pointers to where your results
%   need to go.
% 
%   SynchronizeThreads should be used before returning to MATLAB from your
%   MEX file otherwise there will be fun results.
% 
%   And then compile your MEX file with:
%      mex -R2018a yourmex.c
%
%   Where yourmex.c should be replaced with the name of the mex file you
%   wrote.  
% 
%   To destroy the thread pool and free up memory, all you need to do is
%   call CLEAR ALL (not just clear) from the MATLAB command prompt or in
%   your script or function.  Memory will also be cleared properly upon
%   exiting MATLAB.
% 
%   See also MADD_THREADPOOL.c as an example on how to use things.
% 
%   This might work on MATLAB versions older than R2018a, but I only use
%   -R2018a to compile my MEX files 'cause that's the way I like it so
%   I can only say it works on R2018a and above.
