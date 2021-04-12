function R = utrandi(varargin)
% Generate random integers for unit-testing, range is 0..100

R = randi([0, 100], varargin{:});

