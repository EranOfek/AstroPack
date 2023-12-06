function Flag=select_maxima(MaxVal,varargin)
% Select some maxima out of list by some criteria
% Package: imUtil.patternMatch
% Description: Given a list of maxima, select a sub list by some criteria.
%              For example, can be used to select N highest peaks, or all
%              peaks above some threshold, and larger than some fraction of
%              the highest peak.
% Input  : - A vector of maxima values.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'MaxMethod' - The method by which the 2D histogram peaks will
%                     be selected. The following options are available:
%                     'thresh' - Select maxima larger than some threshold.
%                     'max1' - Select the highest maxima.
%                     'maxall' - Select all the maxima.
%                     'max_fracmax' - Select all the maxima above a
%                               fraction (given by FracOfMax) of the
%                               highest maximum.
%                     'thresh_fracmax' - Select all the maxima above the
%                               threshold and above a
%                               fraction (given by FracOfMax) of the
%                               highest maximum.
%                     Alternatively this can be a positive integer (N).
%                     In this case will return the N highest maxima.
%                     Default is 'thresh'
%            'Threshold' - Peak selection threshold
%                     Default is 5.
%            'FracOfMax' - The parameter that that used in '*_fracmax' and
%                     for selecting peaks.
%                     Only peaks that above the maximal peak multiplied by
%                     this parameter will be returned.
% Output : - A vector of logicals indicating the selected peaks.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Flag=imUtil.patternMatch.select_maxima(randn(100,1).*2);
% Reliable: 2


InPar = inputParser;

addOptional(InPar,'MaxMethod','thresh');
addOptional(InPar,'Threshold',5);
addOptional(InPar,'FracOfMax',0.5);

parse(InPar,varargin{:});
InPar = InPar.Results;



switch lower(InPar.MaxMethod)
    case 'max1'
        % select the highest peak only
        [~,Ind] = max(MaxVal);
        Flag    = false(size(MaxVal));
        Flag(Ind) = true;
    case 'maxall'
        % return all maxima
        Flag = true(size(MaxVal));
    case 'thresh'
        % select by S/N>Threshold
        Flag = MaxVal>InPar.Threshold;
    case 'thresh_fracmax'
        % select by S/N>Threshold and S/N>max(SN)*FracOfMax
        Flag = MaxVal>InPar.Threshold & MaxVal>(max(MaxVal).*InPar.FracOfMax);
    case 'max_fracmax'
        % select all above FracOfMax
        Flag = MaxVal>(max(MaxVal).*InPar.FracOfMax);
    otherwise
        if isnumeric(InPar.MaxMethod)
            % select N highest peaks
            [~,SI] = sort(MaxVal,'descend');
            
            Npeak = min(InPar.MaxMethod,numel(MaxVal));
            Ind = SI(1:Npeak);
            Flag    = false(size(MaxVal));
            Flag(Ind) = true;
        else
            error('Unknown MaxMethod option');
        end
end
            
            

