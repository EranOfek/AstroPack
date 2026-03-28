% Fast polynomial radial-distortion term evaluation for X (or Y) coordinates.
%   For the case of continous integer powers, this function can be x40
%   faster than matlab.
%
%   Evaluates a polynomial distortion model of the form
%       Xd = sum(CoefX(:) .* ((X(:).').^X_Xpower(:)) .* ...
%                        ((Y(:).').^X_Ypower(:)) .* ...
%                        ((R(:).').^X_Rpower(:)), 1);
%       Xd = reshape(Xd, size(X));
%   where the corresponding syntax is:
%       Xd = imUtil.trans.mex.polyRadialDistortion1(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower)
%
%   See also (not compiled version with no AVX2): 
%       polyRadialDistortion_noAVX2.cpp
% Input  : - (X) Input X values. Numeric array of class single or double.
%          - (Y) Input Y values. Numeric array of class single or double.
%            Must contain the same number of elements as X.
%          - (R) Radius values. Numeric array of class single or double.
%                 Must be either:
%                   1) a scalar, or
%                   2) an array with the same number of elements as X.
%          - (CoefX) Coefficient vector. Numeric array of class single or double.
%                 Each element defines one polynomial term.
%          - (X_Xpower) Powers of X for each term. Numeric array of the same class
%                 as X. Must contain numel(CoefX) elements.
%          - (X_Ypower) Powers of Y for each term. Numeric array of the same class
%                 as X. Must contain numel(CoefX) elements.
%          - (X_Rpower) Powers of R for each term. Numeric array of the same class
%                 as X. Must contain numel(CoefX) elements.
%
% Output : - (Xd) Output array of the same size and class as X.
%            Internally, the computation treats X, Y, and R as linearized
%            arrays, but the result is reshaped back to the size of X.
%
% Requirements
%   - All inputs must be real.
%   - All numeric inputs must be of the same class: either all single or all double.
%   - X and Y must have the same number of elements.
%   - R must be scalar or have the same number of elements as X.
%   - CoefX, X_Xpower, X_Ypower, and X_Rpower must all have the same number
%     of elements.
%
% Internal optimization branches
%   The MEX code selects automatically between several internal branches:
%
%   1) R == 1 scalar branch
%      If R is a scalar equal to 1, then the factor R.^X_Rpower is always 1,
%      so it is skipped completely.
%
%   2) Scalar-R branch
%      If R is scalar but not equal to 1, then R.^X_Rpower is computed once
%      per polynomial term and absorbed into the coefficients, instead of
%      recomputing it for every element of X and Y.
%
%   3) Small-integer powers branch
%      If all powers in X_Xpower, X_Ypower, and X_Rpower are integers in the
%      range 0..5, then the code replaces general pow calls by explicit
%      multiplications, which is usually much faster.
%
%   4) Consecutive-integer sequence branch
%      If one or more of the power vectors is an equally spaced sequence of
%      integers with step 1, starting at 0 or 1, for example
%      [0 1 2 3 ...] or [1 2 3 4 ...], then powers are generated recursively
%      by repeated multiplication instead of evaluating each power separately.
%
%   5) AVX2 SIMD branch
%      For suitable cases, mainly the small-integer and sequence branches,
%      the code uses AVX2 vector instructions to process several elements of
%      X, Y, and R simultaneously.
%
%   6) Generic fallback branch
%      If none of the faster special cases applies, the code uses a general
%      implementation based on standard power evaluation.
%
% Notes
%   - The output is returned as a row vector, regardless of the shape of X.
%   - Branch selection is automatic; no user action is required.
%   - The fastest cases are usually:
%       * R = 1
%       * scalar R
%       * small integer exponents
%       * consecutive exponent sequences
%
% Compilation: mex -R2018a CXXFLAGS='$CXXFLAGS -O3 -fopenmp -mavx2 -mfma -march=native' LDFLAGS='$LDFLAGS -fopenmp' polyRadialDistortion5.cpp
% Author : ChatGPT + Eran Ofek (Mar 2026)
% Example : General use
%   X = rand(100,1);
%   Y = rand(100,1);
%   R = sqrt(X.^2 + Y.^2);
%   CoefX    = [1; 2; 3];
%   X_Xpower = [1; 0; 2];
%   X_Ypower = [0; 1; 1];
%   X_Rpower = [0; 2; 1];
%   Xd = polyRadialDistortion1(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower);
%
%   X = rand(1,1000);
%   Y = rand(1,1000);
%   R = 1;
%   CoefX    = [0.1; -0.02; 0.003];
%   X_Xpower = [1; 3; 1];
%   X_Ypower = [0; 0; 2];
%   X_Rpower = [0; 0; 0];
%
%   Xd = polyRadialDistortion1(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower);
%
%   % Verification against MATLAB expression
%   X = rand(1,1000,'single');
%   Y = rand(1,1000,'single');
%   R = sqrt(X.^2 + Y.^2);
%   CoefX    = single([1; 2; 3]);
%   X_Xpower = single([1; 0; 2]);
%   X_Ypower = single([0; 1; 1]);
%   X_Rpower = single([0; 2; 1]);
%   Xd1 = polyRadialDistortion1(X, Y, R, CoefX, X_Xpower, X_Ypower, X_Rpower);
%   Xd2 = sum(CoefX(:) .* ((X(:).').^X_Xpower(:)) .* ...
%                       ((Y(:).').^X_Ypower(:)) .* ...
%                       ((R(:).').^X_Rpower(:)), 1);
%   max(abs(double(Xd1) - double(Xd2)))