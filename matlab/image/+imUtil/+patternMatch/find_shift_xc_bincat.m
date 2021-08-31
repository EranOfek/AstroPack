function Res=find_shift_xc_bincat(Cat,Ref,varargin)
% find best X/Y shift between two catalogs from xcorr binned catalogs
% Package: imUtil.patternMatch
% Description: Find best X/Y shift between two catalogs that contains [X,Y]
%              positions. The shifts are identified by calculating the
%              histogram of each catalog (into an effective image), and
%              cross correlating the two histograms.
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
%                     If empty, then will select automaticall the maximal
%                     possible range.
%                     Alternatively, this can be a vector of X-axis
%                     histogram edges.
%                     Default is [-1000 1000].
%            'RangeY' - [Min Max] range of Y shifts to test.
%                     If empty, then will select automaticall the maximal
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
%                     For details see imUtil.image.local_maxima.
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
%            'OverlapXY' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is [16 16].
%            'MinVariance' - The minimum variance in in the 2D histogram,
%                     That is used to calculate the S/N.
%                     Default is 1.
%            'CatColX' - Catalog column that contains the X axis. Default is 1.
%            'CatColY' - Catalog column that contains the Y axis. Default is 2.
%            'RefColX' - Reference column that contains the X axis. Default is 1.
%            'RefColY' - Reference column that contains the Y axis. Default is 2.
% Output : - A structure that contains a list of all possible shifts that
%            can explain the data. The following fields are available:
%            'MaxX' - A vector of X shifts one need to add to the catalog
%                     in order to get the reference (after the flip is
%                     applied to the catalog).
%            'MaxY' - A vector of Y shifts one need to add to the catalog
%                     in order to get the reference (after the flip is
%                     applied to the catalog).
%            'MaxVal' - Number of matched sources found in the histogram
%                     peak. One entry per source.
%            'MaxSN'   - S/N for each peak.
%            'Flip'  - A two column matrix of flip [X,Y] corresponding to
%                     each peak.
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
    Ref = rand(Nstar,2).*2048;
    Ref = sortrows(Ref,2);
    Noverlap = 100;
    Cat = [Ref(1:Noverlap,1), Ref(1:Noverlap,2)];
    Cat = [Cat; rand(Nstar-Noverlap,2).*2048];
    Cat(:,1) = Cat(:,1) + 520 + randn(Nstar,1).*0.3;
    Cat(:,2) = Cat(:,2) + 430 + randn(Nstar,1).*0.3;
    Cat      = sortrows(Cat,2);
    
end


InPar = inputParser;

addOptional(InPar,'Flip',[1 1]); %[1 1; 1 -1;-1 -1; -1 1]);
addOptional(InPar,'CatCCDSEC',[]);
addOptional(InPar,'RefCCDSEC',[]);

addOptional(InPar,'RangeX',[-1000 1000]);
addOptional(InPar,'RangeY',[-1000 1000]);
addOptional(InPar,'StepX',3);
addOptional(InPar,'StepY',3);

addOptional(InPar,'xcorr_fftPar',{});




addOptional(InPar,'MaxMethod','thresh_fracmax');  % 'max1' | 'maxall' | 'max1frac' | 'sn'
addOptional(InPar,'Threshold',5);  % if 'sn', this is S/N if 'maxn'|'max1' this is N stars
addOptional(InPar,'Conn',8);
addOptional(InPar,'FracOfMax',0.5);

addOptional(InPar,'BackFun',@median); % @median);
addOptional(InPar,'BackFunPar',{'all','omitnan'});      % {[1 2],'omitnan'});
addOptional(InPar,'VarFun',@imUtil.background.rvar);    % if empty, then will try to read var from second output of BackFun...
addOptional(InPar,'VarFunPar',{}); % {[1 2]});
addOptional(InPar,'SubSizeXY',[128 128]);  % or 'full'
addOptional(InPar,'OverlapXY',[16 16]); 
addOptional(InPar,'MinVariance',1);

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

if isempty(InPar.CatCCDSEC)
    % select CCDSEC of Cat based on min/max
    InPar.CatCCDSEC = [min(Cat(:,InPar.CatColX)), max(Cat(:,InPar.CatColX)), min(Cat(:,InPar.CatColY)), max(Cat(:,InPar.CatColY))];
end
if isempty(InPar.RefCCDSEC)
    % select CCDSEC of Ref based on min/max
    InPar.RefCCDSEC = [min(Ref(:,InPar.RefColX)), max(Ref(:,InPar.RefColX)), min(Ref(:,InPar.RefColY)), max(Ref(:,InPar.RefColY))];
end


% generate an historam-image of each catalog
[HistCat,CatVecX,CatVecY] = imUtil.patternMatch.hist2d(Cat(:,InPar.CatColX),Cat(:,InPar.CatColY),...
                                    InPar.CatCCDSEC(1:2),InPar.CatCCDSEC(3:4),InPar.StepX,InPar.StepY);
[HistRef,RefVecX,RefVecY] = imUtil.patternMatch.hist2d(Ref(:,InPar.RefColX),Ref(:,InPar.RefColY),...
                                    InPar.RefCCDSEC(1:2),InPar.RefCCDSEC(3:4),InPar.StepX,InPar.StepY);



% init
Res.MaxX   = [];
Res.MaxY   = [];
Res.MaxVal = [];
Res.MaxSN  = [];
Res.Flip   = zeros(0,2);

Nflip = size(InPar.Flip,1);
for Iflip=1:1:Nflip

    
    % flip hist images
    if all(InPar.Flip(Iflip,:)==[1 1])
        HistCatF = HistCat;
    elseif all(InPar.Flip(Iflip,:)==[-1 1])
        HistCatF = fliplr(HistCat);
        VecXF    = flip(VecX);
    elseif all(InPar.Flip(Iflip,:)==[1 -1])
        HistCatF = flipud(HistCat);
        VecYF    = flip(VecY);
    elseif all(InPar.Flip(Iflip,:)==[-1 -1])
        HistCatF = flipud(HistCat);
        HistCatF = fliplr(HistCatF);
        VecXF    = flip(VecX);
        VecYF    = flip(VecY);
    else
        error('Unknown Flip option');
    end
        
    [XC,Res,ResPeak] = imUtil.filter.xcorr_fft(HistCatF,HistRef,InPar.xcorr_fftPar{:});
    
    got here
    
    
    
    
    % Vectors of X/Y coordinates
    Xcat = Cat(:,InPar.CatColX).*InPar.Flip(Iflip,1);
    Ycat = Cat(:,InPar.CatColY).*InPar.Flip(Iflip,2);
    Xref = Ref(:,InPar.RefColX);
    Yref = Ref(:,InPar.RefColY);


    
    % calculate matrices of X and Y distances
    Dx = Xcat - Xref.'; % Xref - Xcat.';
    Dy = Ycat - Yref.'; %Yref - Ycat.';

    if isempty(InPar.RangeX)
        InPar.RangeX = [min(Dx(:)), max(Dx(:))];
    end
    if isempty(InPar.RangeY)
        InPar.RangeY = [min(Dy(:)), max(Dy(:))];
    end
    
    
    % generate a 2D histogram of X and Y distances
    [H2,VecX,VecY] = imUtil.patternMatch.hist2d(Dx(:),Dy(:),InPar.RangeX,InPar.RangeY,InPar.StepX,InPar.StepY);
    
    
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
                                                       'OverlapXY',InPar.OverlapXY);

                    % normalize the H2 histogram surface by expected region of pverlap.
                    Var = max(Var,InPar.MinVariance);
                    H2 = (H2 - Back)./sqrt(Var);
            end

                    
            [~,Pos]=imUtil.image.local_maxima(H2,1,InPar.Threshold,InPar.Conn);
            Pos = sortrows(Pos,3,'descend');
            Flag = imUtil.patternMatch.select_maxima(Pos(:,3),'MaxMethod',InPar.MaxMethod,'FracOfMax',InPar.FracOfMax,'Threshold',InPar.Threshold);
            
            MaxX   = VecX(Pos(Flag,1));
            MaxY   = VecY(Pos(Flag,2));
            MaxVal = Pos(Flag,3);
            MaxSN  = MaxVal./max(std(H2,[],'all'),sqrt(InPar.MinVariance));
    end
      
    Res.MaxX   = [Res.MaxX; MaxX];
    Res.MaxY   = [Res.MaxY; MaxY];
    Res.MaxVal = [Res.MaxVal; MaxVal];
    Res.MaxSN  = [Res.MaxSN; MaxSN];
    Res.Flip   = [Res.Flip; InPar.Flip(Iflip,:).*ones(numel(MaxX),1)];
    
end


Flag = imUtil.patternMatch.select_maxima(Res.MaxVal,'MaxMethod',InPar.MaxMethod,'FracOfMax',InPar.FracOfMax,'Threshold',InPar.Threshold);
Res.MaxX   = Res.MaxX(Flag);
Res.MaxY   = Res.MaxY(Flag);
Res.MaxVal = Res.MaxVal(Flag);
Res.MaxSN  = Res.MaxSN(Flag);
Res.Flip   = Res.Flip(Flag,:);
