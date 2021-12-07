function [Res,H2]=find_shift_pairs(Cat,Ref,varargin)
% find best X/Y shift between two catalogs from all pairs differences
% Package: imUtil.patternMatch
% Description: Find best X/Y shift between two catalogs that contains [X,Y]
%              positions. The shifts are identified by calculating the
%              histogram of all the X differences between the two catalogs
%              and the Y differences between the two catalogs.
%              Next, search for peaks in the 2D histogram using various
%              methods, and return all the peaks that satisfy some
%              selection criteria.
%              The returned shifts are the offset one need to add to the
%              Catalog in order to get the Referece.
%              If flip is applied, then the flip need to be applied on the
%              catalog before the shift is applied.
% Input  : - A catalog. A matrix in which two columns (default is 1 and 2)
%            contains the X and Y position of the sources.
%            If no parameters are provided, then will run in
%            simulation/test mode.
%          - A reference catalog. A matrix in which two columns
%            (default is 1 and 2)
%            contains the X and Y position of the sources.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'Flip' - A two column matrix of all possible flips to test.
%                     Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].
%                     Default is [1 1].
%            'RangeX' - [Min Max] range of X shifts to test.
%                     If empty, then will select automatically the maximal
%                     possible range.
%                     Alternatively, this can be a vector of X-axis
%                     histogram edges.
%                     Default is [-1000 1000].
%            'RangeY' - [Min Max] range of Y shifts to test.
%                     If empty, then will select automatically the maximal
%                     possible range.
%                     Alternatively, this can be a vector of Y-axis
%                     histogram edges.
%                     Default is [-1000 1000].
%            'StepX' - X-axis step size for the histogram calculation.
%                     If empty, then will use RangeX as a vector of edges.
%                     Default is 3.
%            'StepY' - Y-axis step size for the histogram calculation.
%                     If empty, then will use RangeY as a vector of edges.
%                     Default is 3.
%            'MaxMethod' - he method by which the 2D histogram peaks will
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
%                     Default is 'thresh_fracmax'
%            'Threshold' - Detection threshold.
%                     If PeakMethod is 'sn' then this has units of S/N.
%                     Otherwise, this is the number of matches.
%                     Default is 8.
%            'Conn' - local maxima finding connectivity parameter.
%                     For details see imUtil.sources.findLocalMax.
%                     Default is 8.
%            'FracOfMax' - The parameter that that used in 'max1frac' and
%                     'sn' PeakMethod, for selecting peaks.
%                     Only peaks that above the maximal peak multiplied by
%                     this parameter will be returned.
%            'BackFun' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is @median.
%            'BackFunPar' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is {'all','omitnan'}.
%            'VarFun' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is @imUtil.background.rvar.
%            'VarFunPar' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is {}.
%            'SubSizeXY' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is [128 128].
%            'Overlap' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is [16 16].
%            'MinVariance' - The minimum variance in in the 2D histogram,
%                     That is used to calculate the S/N.
%                     Default is 1.
%            'FilterSigma' - Width [sigma units] of Gaussian filter with
%                   which to cross-correlate the H2 (hits) matrix.
%                   If empty, no filtering is applied. Default is 3.
%            'CatColX' - Catalog column that contains the X axis. Default is 1.
%            'CatColY' - Catalog column that contains the Y axis. Default is 2.
%            'RefColX' - Reference column that contains the X axis. Default is 1.
%            'RefColY' - Reference column that contains the Y axis. Default is 2.
% Output : - A structure that contains a list of all possible shifts that
%            can explain the data. The following fields are available:
%            'ShiftX' - A vector of X shifts one need to add to the
%                     reference
%                     in order to get the catalog (after the flip is
%                     applied to the reference).
%            'ShiftY' - A vector of Y shifts one need to add to the
%                     reference
%                     in order to get the catalog (after the flip is
%                     applied to the reference).
%            'MaxVal' - Number of matched sources found in the histogram
%                     peak. One entry per source.
%            'SN'    - S/N for each peak.
%            'Flip'  - A two column matrix of flip [X,Y] corresponding to
%                     each peak. This flip needed to be applied to Ref in
%                     order to get Cat.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Res=imUtil.patternMatch.find_shift_pairs;
% Reliable: 2
%--------------------------------------------------------------------------


if (nargin<2)
    % simulation mode
    
    % simulation mode
    Nstar = 1000;
    Ref = rand(Nstar,2).*2048 - 1024;
    Ref = sortrows(Ref,2);
    Noverlap = 100;
    Cat = [Ref(1:Noverlap,1), Ref(1:Noverlap,2)];
    Cat = [Cat; rand(Nstar-Noverlap,2).*1024];
%     Cat(:,1) = Cat(:,1) + 520 + randn(Nstar,1).*0.3;
%     Cat(:,2) = Cat(:,2) - 430 + randn(Nstar,1).*0.3;
%     Cat      = sortrows(Cat,2);
%     Cat      = Cat.*[1 -1];
    
     Tr = [0, 1, -120, 43.9, 1, -1];
    [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,Tr,'+');
    Cat = [NewX, NewY];
    
end


InPar = inputParser;

addOptional(InPar,'Flip',[1 1; 1 -1;-1 -1; -1 1]);
addOptional(InPar,'RangeX',[-1000 1000]);
addOptional(InPar,'RangeY',[-1000 1000]);
addOptional(InPar,'StepX',3);
addOptional(InPar,'StepY',3);
addOptional(InPar,'MaxMethod','thresh_fracmax');  % 'max1' | 'maxall' | 'max1frac' | 'sn'
addOptional(InPar,'Threshold',5);  % if 'sn', this is S/N if 'maxn'|'max1' this is N stars
addOptional(InPar,'Conn',8);
addOptional(InPar,'FracOfMax',0.5);

addOptional(InPar,'BackFun',@median); % @median);
addOptional(InPar,'BackFunPar',{'all','omitnan'});      % {[1 2],'omitnan'});
addOptional(InPar,'VarFun',@imUtil.background.rvar);    % if empty, then will try to read var from second output of BackFun...
addOptional(InPar,'VarFunPar',{}); %{[],[1 2]});
addOptional(InPar,'SubSizeXY',[128 128]);  % or 'full'
addOptional(InPar,'Overlap',[16]); 
addOptional(InPar,'MinVariance',1);
addOptional(InPar,'FilterSigma',3);

addOptional(InPar,'CatColX',1);
addOptional(InPar,'CatColY',2);
addOptional(InPar,'RefColX',1);
addOptional(InPar,'RefColY',2);

parse(InPar,varargin{:});
InPar = InPar.Results;

if ischar(InPar.Flip)
    % assume Flip is all
    InPar.Flip = [1 1; 1 -1;-1 -1; -1 1];
end

% init
Res.ShiftX = [];
Res.ShiftY = [];
Res.MaxVal = [];
Res.SN     = [];
Res.Flip   = zeros(0,2);

% X/Y coordinates in Cat:
Xcat = Cat(:,InPar.CatColX);
Ycat = Cat(:,InPar.CatColY);

Nflip = size(InPar.Flip,1);
for Iflip=1:1:Nflip

    % Vectors of X/Y coordinates
    Xref = Ref(:,InPar.RefColX).*InPar.Flip(Iflip,1);
    Yref = Ref(:,InPar.RefColY).*InPar.Flip(Iflip,2);
    
    % calculate matrices of X and Y distances
    Dx = Xcat - Xref.'; % Xref - Xcat.';
    Dy = Ycat - Yref.'; %Yref - Ycat.';

    if isempty(InPar.RangeX)
        InPar.RangeX = [min(Dx(:)), max(Dx(:))];Res.Flip(Isol,:),...
    end
    if isempty(InPar.RangeY)
        InPar.RangeY = [min(Dy(:)), max(Dy(:))];
    end
    
    
    % generate a 2D histogram of X and Y distances
    % slower
    %[H2,VecX,VecY] = imUtil.patternMatch.hist2d(Dx(:),Dy(:),InPar.RangeX,InPar.RangeY,InPar.StepX,InPar.StepY);
    % Note that X/Y are reversed!
    [H2,VecY,VecX] = tools.array.hist2d_fast(Dy(:),Dx(:),InPar.RangeY,InPar.RangeX,InPar.StepY,InPar.StepX);
    
    
    % filter H2
    if ~isempty(InPar.FilterSigma)
        Gaussian = imUtil.kernel2.gauss(InPar.FilterSigma);
        %Gaussian = Gaussian./sqrt(sum(Gaussian(:).^2));
        Gaussian = Gaussian./norm(Gaussian(:)); % faster
        
        H2       = imUtil.filter.filter2_fast(H2, Gaussian);
    end
    
    % Fix the sign of VecX, VecY according to the Flip
    VecX = VecX.*InPar.Flip(Iflip,1);
    VecY = VecY.*InPar.Flip(Iflip,2);
    
    
    switch InPar.MaxMethod
        case 'max1'
            % select highest peak above threshold
            [MaxVal,MaxInd] = max(H2(:));
            if ~isempty(MaxInd)
                [MaxY,MaxX] = imUtil.image.ind2sub_fast(size(H2),MaxInd);
                MaxX        = VecX(MaxX);
                MaxY        = VecY(MaxY);
                MaxSN       = MaxVal./max(std(H2,[],'all'),sqrt(InPar.MinVariance));
            end
           
        otherwise
            switch lower(InPar.MaxMethod)
                case {'thresh','thresh_fracmax'}
                    [Back,Var]=imUtil.background.background(H2,...
                                                       'BackFun',InPar.BackFun,...
                                                       'BackFunPar',InPar.BackFunPar,...
                                                       'VarFun',InPar.VarFun,...
                                                       'VarFunPar',InPar.VarFunPar,...
                                                       'SubSizeXY',InPar.SubSizeXY,...
                                                       'Overlap',InPar.Overlap);

                    % normalize the H2 histogram surface by expected region of pverlap.
                    Var = max(Var,InPar.MinVariance);
                    H2 = (H2 - Back)./sqrt(Var);
            end

                    
            %[~,Pos]=imUtil.image.local_maxima(H2,1,InPar.Threshold,InPar.Conn);
            % much faster:
            [Pos] = imUtil.sources.findLocalMax(H2, 'Variance',1, 'Threshold',InPar.Threshold,'Conn',InPar.Conn, 'Algo','findlocal');

            Pos = sortrows(Pos,3,'descend');
            Flag = imUtil.patternMatch.select_maxima(Pos(:,3),'MaxMethod',InPar.MaxMethod,'FracOfMax',InPar.FracOfMax,'Threshold',InPar.Threshold);
            
            MaxX   = VecX(Pos(Flag,1));
            MaxY   = VecY(Pos(Flag,2));
            MaxVal = Pos(Flag,3);
            MaxSN  = MaxVal./max(std(H2,[],'all'),sqrt(InPar.MinVariance));
    end
    
    % The following is needed in order to make the shift consistent
    % with the way it is applied in affine2d_transformation
    MaxX = MaxX.*InPar.Flip(Iflip,1);
    MaxY = MaxY.*InPar.Flip(Iflip,2);
    
    Res.ShiftX  = [Res.ShiftX; MaxX];
    Res.ShiftY  = [Res.ShiftY; MaxY];
    Res.MaxVal  = [Res.MaxVal; MaxVal];
    Res.SN      = [Res.SN; MaxSN];
    Res.Flip    = [Res.Flip; InPar.Flip(Iflip,:).*ones(numel(MaxX),1)];
    
end


Flag = imUtil.patternMatch.select_maxima(Res.MaxVal,'MaxMethod',InPar.MaxMethod,'FracOfMax',InPar.FracOfMax,'Threshold',InPar.Threshold);
Res.ShiftX = Res.ShiftX(Flag);
Res.ShiftY = Res.ShiftY(Flag);
Res.MaxVal = Res.MaxVal(Flag);
Res.SN     = Res.SN(Flag);
Res.Flip   = Res.Flip(Flag,:);
