# Simpler MEX Multi-Threading w/ a Persistent Thread Pool

[![View Simpler MEX Multi-Threading w/ a Persistent Thread Pool on File Exchange](https://www.mathworks.com/matlabcentral/images/matlab-file-exchange.svg)](https://www.mathworks.com/matlabcentral/fileexchange/71532-simpler-mex-multi-threading-w-a-persistent-thread-pool)

I can't believe this actually worked.

For smaller problems that can't be vectorized, properly setting up multi-threading within a MEX file can create enough overhead that the single-threaded solution is often faster. Using a thread pool eliminates that overhead. The pool is created once, and then can be re-used in multiple calls to your MEX function. The thread pool is then closed out and cleared from memory by typing 'clear all' or 'clear mex' in the command line (or closing MATLAB).

Depending on the problem, you can potentially see a speedup of up to 'maxNumCompThreads', though that's probably not likely. In my very basic testing, I'm seeing ~2x speedup (with four threads) over many iterations of a small problem. For the real-world problem I created this for, I'm seeing a ~60% speedup compared to standard MEX multi-threading, saving me a couple of days in total simulation time.

In addition, adding multi-threading to a MEX file is now much easier, as you only need to learn how to use one function, AddThreadPoolJob. You then just have to remember to close your MEX file with SynchronizeThreads so that you don't get awesome race conditions.

See the included madd_threadpool.c example and compare to madd.c that uses regular multi-threading to figure out what's going on. There's also more documentation in threadpool.h. To see how single- thread vs. multi-thread vs. thread pooled vs. vectorized code compares, compile things and check out threadpool_timings.m.

NOTE: properly vectorized MATLAB code will always be faster than this. Only use this stuff if you can't vectorize your code but it still lends itself to straightforward parallelization, e.g. integrating recurrence relations in multiple dimensions.
