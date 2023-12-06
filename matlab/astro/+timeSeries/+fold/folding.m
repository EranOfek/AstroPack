function [Result, RefT0] = folding(LC, Period, Args)
% Folding a time series by some period
% Package: timeSeries.fold
% Description: Folding a time series into a period, and calculate the phase
%              for each data point in the time series.
% Input  : - Matrix or table in which one of the columns is the time.
%          - period to fold into.
%          * ...,key,val,...
%            'ColT' - Index of time column in input matrix.
%                   Default is 1.
%            'RefT0' - Reference t0 to subtract from time (time of
%                   phase=0). If a function handle then will execute this
%                   function on ther time column. Default is @median.
%            'SortT' - a logical indicating if to sort the output by phase.
%                   Default is false.
% Output : - Matrix similar to the input matrix, but in which
%            the time column is replaced by the phase.
%          - Reference t0 used.
% Tested : Matlab 3.5
% Author : Eran Ofek (Nov 1993)
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: timeSeries.fold.folding(rand(100,2).*100,17);
% Reliable: 1

arguments
    LC
    Period
    Args.ColT          = 1;
    Args.RefT0         = @median;
    Args.SortT logical = false;
end

if isa(Args.RefT0, 'function_handle')
    if istable(LC)
        RefT0 = Args.RefT0(table2array(LC(:,Args.ColT)));
    else
        RefT0 = Args.RefT0(LC(:,Args.ColT));
    end
else
    RefT0 = Args.RefT0;
end
    
if istable(LC)
    ColName = LC.Properties.VariableNames{Args.ColT};
    T = LC.(ColName) - RefT0;
else
    T     = LC(:,Args.ColT) - RefT0;
end
TP    = T./Period;
Phase = TP - floor(TP);
Result = LC;
if istable(LC)
    Result.(ColName) = Phase;
else
    Result(:,Args.ColT) = Phase;
end

if Args.SortT
    if istable(LC)
        Result = sortrows(Result, ColName);
    else
        Result = sortrows(Result, Args.ColT);
    end
end

%% old version
%if nargin==2,
%   c=1;
%elseif nargin==3,
%   % do nothing
%else
%   error('Illegal number of input arguments');
%end
%
%jd_col      = length(x(1,:)) + 1;
%y           = zeros(length(x(:,1)),jd_col);
%TEMP        = x(:,c);
%r           = TEMP;
%TEMP        = TEMP./p-floor(TEMP./p);
%y           = x;
%y(:,c:c)    = TEMP;
%y(:,jd_col) = r;
%y           = sortrows(y,c);
