function stats = array_stats_wrapper(A)
% ARRAY_STATS_WRAPPER  Compute descriptive statistics using the array_stats MEX function.
%
%   STATS = ARRAY_STATS_WRAPPER(A) returns a struct with fields:
%       .mean          — arithmetic mean of non-NaN elements
%       .std_dev       — sample standard deviation (N-1 denominator) of non-NaN elements
%       .min_val       — minimum of non-NaN elements
%       .max_val       — maximum of non-NaN elements
%       .element_count — total number of elements (including NaN)
%
%   Input:
%       A — real, non-sparse double array (1D or 2D)
%
%   Notes on NaN handling:
%       NaN values are excluded from mean, std_dev, min, and max calculations.
%       element_count always reflects the total size of A.
%       If ALL elements are NaN, mean/std_dev/min/max will be NaN.
%
%   Examples:
%       s = array_stats_wrapper([1 2 3 4 5]);
%       fprintf('mean=%.4f  std=%.4f  min=%.4f  max=%.4f  n=%d\n', ...
%               s.mean, s.std_dev, s.min_val, s.max_val, s.element_count);
%
%       s = array_stats_wrapper(magic(4));
%       fprintf('mean of magic(4) = %.4f\n', s.mean);

    % --- Input validation (MATLAB side, before calling MEX) ---------------
    if nargin ~= 1
        error('array_stats_wrapper:badNargin', ...
              'Expected exactly 1 input argument, got %d.', nargin);
    end

    if ~isnumeric(A)
        error('array_stats_wrapper:notNumeric', ...
              'Input must be numeric. Got class: %s.', class(A));
    end

    if ~isa(A, 'double')
        error('array_stats_wrapper:notDouble', ...
              'Input must be a double array. Got class: %s. Use double(A) to convert.', class(A));
    end

    if ~isreal(A)
        error('array_stats_wrapper:isComplex', ...
              'Input must be real (non-complex).');
    end

    if issparse(A)
        error('array_stats_wrapper:isSparse', ...
              'Input must be a full (non-sparse) array. Use full(A) to convert.');
    end

    if isempty(A)
        error('array_stats_wrapper:emptyArray', ...
              'Input array must not be empty.');
    end

    if ~ismatrix(A)
        error('array_stats_wrapper:notMatrix', ...
              'Input must be 1D or 2D. Got array with %d dimensions.', ndims(A));
    end

    % --- Call MEX ---------------------------------------------------------
    [m, sd, mn, mx, cnt] = array_stats(A);

    % --- Package into struct ----------------------------------------------
    stats.mean          = m;
    stats.std_dev       = sd;
    stats.min_val       = mn;
    stats.max_val       = mx;
    stats.element_count = cnt;
end
