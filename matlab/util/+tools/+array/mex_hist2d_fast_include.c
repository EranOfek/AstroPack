//function [Mat, VecX, VecY, BinX, BinY] = hist2d_fast(Xv,Yv, Xedges, Yedges, StepX, StepY)
    // A fast version of histcounts2 (without all the overheads)
    //       This is faster than imUtil.patternMatch.hist2d
    //       BUT unlike, imUtil.patternMatch.hist2d, the returned matrix X/Y
    //       directions I like those of histcounts2.
    // Input  : - A vector of X coordinates.
    //          - A vector of Y coordinates.
    //          - A vector of X edges.
    //          - A vector of Y edges.
    //          - Flag indicating if i/j or x/y output:
    //            true (default) - i,j output (like histcounts2)
    //               X axis is along i-dimension...
    //            false - output like imUtil.patternMatch.hist2.
    // Output : - A matrix of 2-D histogram (X vs Y).
    //          - Vector of X coordinate of center of X bins. (unlike
    //            histcounts2).
    //          - Vector of Y coordinate of center of Y bins.
    // Author : Eran Ofek (Sep 2021)
    // Example: Xv = rand(1e4, 1); Yv=rand(1e4, 1);
    //          RangeX = [0 1]; RangeY = [0 1.1]; StepX = 0.001; StepY = 0.001;
    //          Xed = (0:0.001:1); Yed = (0:0.001:1.1);
    //          Mat = tools.array.hist2d_fast(Xv,Yv, Xed, Yed); 
    //          Mat0 = tools.array.hist2d_fast(Xv,Yv, RangeX, RangeY, StepX, StepY); 
    //          Mat1 = histcounts2(Xv,Yv, Xed, Yed); // like Mat
    //          // compare all methods
    //          assert(all(Mat==Mat0,'all'))     
    //          assert(all(Mat==Mat1,'all'))
    //          // simple test
    //          Xv = 0.7; Yv =0.4; Xed=(0:0.333:1); Yed=(0:0.333:1.333);
    //          Mat = tools.array.hist2d_fast(Xv,Yv, Xed, Yed)
    //          // speed
    //          tic; for I=1:1:1000, [Mat1,vx1,vy1,bx1,by1] = tools.array.hist2d_fast(Xv,Yv, RangeX, RangeY, StepX, StepY); end, toc
    //          tic; for I=1:1:1000, [Mat1,vx1,vy1,bx1,by1] = tools.array.hist2d_fast(Xv,Yv, Xed, Yed); end, toc              
    //          tic; for I=1:1:1000, [Mat1,vx1,vy1,bx1,by1] = histcounts2(Xv,Yv, Xed, Yed); end, toc
    
  #include <stdio.h>
#include "mex.h"
#include "matrix.h"
#include <complex.h>
// For Ctrl-C detection http://www.caam.rice.edu/~wy1/links/mex_ctrl_c_trick/
#if defined (_WIN32)
#include <windows.h>
#elif defined (__linux__)
#include <unistd.h>
#endif

#ifdef __cplusplus
extern "C" bool utIsInterruptPending();
#else
extern bool utIsInterruptPending();
#endif

//-------------------------------------------------------------------------
// The gateway function
//
// mexFunction is not a routine you call. Rather, mexFunction is the name
// of the gateway function in C which every MEX function requires.
// When you invoke a MEX function, MATLAB finds and loads the corresponding
// MEX function of the same name. MATLAB then searches for a symbol named
// mexFunction within the MEX function. If it finds one, it calls the MEX
// function using the address of the mexFunction symbol. MATLAB displays
// an error message if it cannot find a routine named mexFunction inside
// the MEX function.
//
// Input Arguments
//
//    int nlhs              - Number of output arguments
//    mxArray* plhs[]       - Output arguments
//    int nrhs              - Number of input arguments
//    const mxArray* prhs[] - Input arguments
//


// Input
void mexFunction(
    int nlhs, mxArray *plhs[],          // Output arguments
    int nrhs, const mxArray *prhs[])    // Input arguments
{
    __FLOAT StepX, StepY;
    __FLOAT *Xv, *Yv, *Xedges, *Yedges;
    int Tot_size;
    mxClassID     class_id;
    const char*   input_type;
    mwSize        input_ndims;
    const mwSize* input_size;
    class_id    = mxGetClassID(prhs[0]);
    Xv       = (__FLOAT*)mxGetData(prhs[0]);
    input_ndims = mxGetNumberOfDimensions(prhs[0]);
    input_size  = mxGetDimensions(prhs[0]);
    input_type  = mxGetClassName(prhs[0]);
    Yv       = (__FLOAT*)mxGetData(prhs[1]);
    Xedges   = (__FLOAT*)mxGetData(prhs[2]);
    Yedges   = (__FLOAT*)mxGetData(prhs[3]);

     if ((nrhs > 4) && mxIsScalar(prhs[4]))
    {
        StepX = (__FLOAT)mxGetScalar(prhs[4]);
     }
    if ((nrhs > 5) && mxIsScalar(prhs[5]))
    {
        StepY = (__FLOAT)mxGetScalar(prhs[5]);
    }

    // Calculating number of elements:
    if (input_ndims>=1)
    {
        Tot_size = 1;
    }
    else
    {
        Tot_size = 0;
    }

    for (int i =0;i<input_ndims;i++)
    {
        Tot_size = Tot_size*(int)input_size[i];
    }

    if isempty(StepX)
    {
        // assume steps may be non-equal
        // switching X and Y to avoid transpose at the end
        [~,BinX] = matlab.internal.math.histcounts(Xv, Xedges);
        [~,BinY] = matlab.internal.math.histcounts(Yv, Yedges);
        CountslenX = numel(Xedges)-1;
        CountslenY = numel(Yedges)-1;
    }
    else
    {
        // assume Xedges contains [Xmin, Xmax]
        // assume Yedges contains [Ymin, Ymax]
        BinX = ceil((Xv - Xedges(1))./StepX);
        BinY = ceil((Yv - Yedges(1))./StepY);
        CountslenX = ceil((Xedges(end) - Xedges(1))./StepX);
        CountslenY = ceil((Yedges(end) - Yedges(1))./StepX);
    }
  
    
    // Filter out NaNs and out-of-range data
    Flag = BinX>0 & BinY>0 & BinX<=CountslenX & BinY<=CountslenY;
    Subs = [BinX(Flag) BinY(Flag)];
    //Subs = [BinX(:) BinY(:)];
    Subs(any(Subs==0, 2),:) = [];
    Mat = accumarray(Subs, ones(size(Subs,1),1), [CountslenX, CountslenY]);

    if nargout>1
        if isempty(StepX)
            VecX = (Xedges(1:end-1) + Xedges(2:end)).*0.5;
            VecY = (Yedges(1:end-1) + Yedges(2:end)).*0.5;
        else
            VecX = (Xedges(1):StepX:(Xedges(end)-StepX)) + 0.5.*StepX;
            VecY = (Yedges(1):StepY:(Yedges(end)-StepY)) + 0.5.*StepY;
        end
        VecX = VecX(:);
        VecY = VecY(:);
    end
end
}