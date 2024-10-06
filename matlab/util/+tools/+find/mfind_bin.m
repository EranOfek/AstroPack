function Im=mfind_bin(X, Vals, UseMex, Type)
% Binary search on a sorted vector running simolutnously on multiple values.
% Package: tools.find
% Description: Multiple value simolutanos binary search.
%              A feature of this program is that it
%              you need to add 1 to the index in order to make sure 
%              the found value is larger than the searched value.
% Input  : - Sorted column vector.
%          - Row vector of values to search.
%          - (UseMex) A logical indicating if to use mex version.
%            Default is false.
%          - (UseMP) A logical indicating if to use open MP.
%            Default is true.
%          - Datatype: {[], 'single','double'}.
%            If [], then use the same type as the first input argument.
%            Default is [].
% Output : - Indices of nearest values.
% Tested : Matlab R2014a
%     By : Eran O. Ofek                    Jan 2015
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Compilation: mex -R2018a CXXFLAGS="\$CXXFLAGS -fopenmp" LDFLAGS="\$LDFLAGS -fopenmp" mex_mfind_bin_double.cpp
% Example: X=sort(rand(1e6,1)); Vals=rand(1,1e5);
%          Im=tools.find.mfind_bin(X,Vals)
% Reference: cite Ofek (2014; ascl:1407.005)
% Reliable: 2
%--------------------------------------------------------------------------

arguments
    X                	            % Input array
    Vals                            % Input array
    UseMex logical   = false;      	% True: Use MEX implementation, False: Use MATLAB implementaion
    %UseMP logical    = true;      	% True: Use threading with OpenMP multi-threading library
    Type             = []; 
end


if UseMex
    if isempty(Type)
        Type = class(X);
    end

    switch Type
        case 'double'
            %Im = tools.find.mex.mex_mfind_bin_double(X,Vals,UseMP);
            Im = tools.find.mex.mex_mfind_bin_double(X,Vals);
        case 'single'
            %Im = tools.find.mex.mex_mfind_bin_single(X,Vals,UseMP);
            Im = tools.find.mex.mex_mfind_bin_single(X,Vals);
        otherwise
            error('tools.find.mfind_bin - Unsupported data type');
    end
else
    Nvals = length(Vals);
    N     = length(X);
    I1    = ones(1,Nvals);
    I2    = N.*ones(1,Nvals);
    Im    = uint32(floor(0.5.*(I1+I2)));
    PrevIm= zeros(size(Im),'uint32');
    
    if (numel(X)<2)
        if (isempty(X))
            Im = uint32([]);
        else
            Im = ones(1,Nvals);
        end
    else
        %Niter = 0;
        while (~all(Im==PrevIm))
            %Niter = Niter+ 1;
            FlagU = Vals>X(Im).';
            FlagD = ~FlagU; %Vals<X(Im).';
            I1(FlagU) = Im(FlagU);
            I1(FlagD) = I1(FlagD);
            I2(FlagU) = I2(FlagU);
            I2(FlagD) = Im(FlagD);
            PrevIm    = Im;
            Im        = uint32(floor(0.5.*(I1+I2)));
        end
    end
end

