function Res=find_scalerot_pairs(Cat,Ref,varargin)
% Find rotation and scale required to match two catalogs using logdist-rot hist
% Package: imUtil.patternMatch
% Description: Find the best rotation and scale and flip (by that order)
%              needed to be applied to a acatalog in order to match it
%              to a reference catalog.
%              The algorithm is using the Kaiser method. The histogram of 
%              the log-distance and angle between stars in the reference is
%              cross correlated with the same histogram for the reference.
% Input  : - A catalog.
%            If no arguments are provided, then run in simulation mode.
%          - A reference catalog.
%          * Pairs of ...,key,val,... The following keywords are avaialble:
%            'Flip' - A two column matrix of all possible flips to test.
%                     Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].
%                     Default is [1 1; 1 -1].
%                     This default covers all options.
%            'Scale' - Range of allowed scale. Default is [0 Inf].
%            'HistDistEdges' - [MinDist, MaxDist, NumberOf points] for
%                     distance histogram.
%                     Default is [10 600 300].
%            'HistRotEdges' - Edges for angle axis of the histogram.
%                     Default is (-90:0.2:90).'.
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
%                     Default is 'thresh_fracmax'
%            'Threshold' - Peak selection threshold
%                     Default is 5.
%            'FracOfMax' - The parameter that that used in '*_fracmax' and
%                     for selecting peaks.
%                     Only peaks that above the maximal peak multiplied by
%                     this parameter will be returned.
%            'CatColX' - Catalog column that contains the X axis. Default is 1.
%            'CatColY' - Catalog column that contains the Y axis. Default is 2.
%            'RefColX' - Reference column that contains the X axis. Default is 1.
%            'RefColY' - Reference column that contains the Y axis. Default is 2.
% Output : - A structure containing the following fields:
%            'SN'  - A vector of the S/N of all selected
%                   cross-correkation peaks.
%            'Rot' - A vector of all rotation angles of all selected
%                   cross-correkation peaks.
%                   The rotation angle is in the range [0 180] deg.
%                   The full range is achived by a combination of Flip and Rot.
%            'Scale' - A vector of scales of all selected
%                   cross-correkation peaks.
%            'Flip' - A two column matrix [FlipX, FlipY] corresponding to
%                   the flips of all selected peaks.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Res=imUtil.patternMatch.find_scalerot_pairs;
% Reliable: 2
%--------------------------------------------------------------------------

RAD = 180./pi;

if (nargin<2)
    % simulation mode
    if (nargin==0)
        Theta = 37;
    else
        Theta = Cat;
    end
    % simulation mode
    Ns = 5000;
    Nm = 2000;
    
    Ref = rand(Ns,2).*2048;
    Cat = Ref(1:Nm,:) + rand(Nm,2).*0.1;
    
    
    Tr = [-120, 1.1, -120, -45, -1, -1];
    [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,Tr,'+');
    Cat = [NewX, NewY];
    
    %Cat = [Cat; rand(Nm,2).*1024];
    
    % Rotate Ref
    %Theta = -10;
%     Rot = [cosd(Theta), -sind(Theta); sind(Theta), cosd(Theta)];
%     
%     Cat = Cat.*[-1 1];
%     
%     Scale = 1.1;
%     Cat = Scale.*(Rot*Cat.').';
    
    
end


InPar = inputParser;


addOptional(InPar,'Flip',[1 1; 1 -1;-1 -1; -1 1]);
addOptional(InPar,'Scale',[0 Inf]); 
addOptional(InPar,'HistDistEdges',[10 600 300]);
addOptional(InPar,'HistRotEdges',(-90:0.2:90).');

addOptional(InPar,'MaxMethod','thresh_fracmax');
addOptional(InPar,'Threshold',5);
addOptional(InPar,'FracOfMax',0.5);
addOptional(InPar,'Conn',8);

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


MaxDist = max(InPar.HistDistEdges);

%'HistDistEdges',InPar.HistDistEdges,...
ThetaShift = InPar.HistRotEdges(:).'; % - min(InPar.HistRotEdges);


% Hist edges for Ref:
StepHist = (log10(InPar.HistDistEdges(2)) - log10(InPar.HistDistEdges(1)))./InPar.HistDistEdges(3);
LogHistDistEdges = (log10(InPar.HistDistEdges(1)):StepHist:log10(InPar.HistDistEdges(2))).';
 


% Cat:
CatX = Cat(:,InPar.CatColX); %.*InPar.Flip(Iflip,1);
CatY = Cat(:,InPar.CatColY); %.*InPar.Flip(Iflip,2);
% all possible differences between sources in Cat
CatDiffX = CatX - CatX.';
CatDiffY = CatY - CatY.';
% select withn Max Dist
Fc = abs(CatDiffX(:))<MaxDist & abs(CatDiffY(:))<MaxDist;
CatDiffX = CatDiffX(Fc);
CatDiffY = CatDiffY(Fc);
% all possible distances/angle between sources in Cat
CatDist  = sqrt(CatDiffX.^2 + CatDiffY.^2);
CatTan   = atan(CatDiffY./CatDiffX);
% calc histograms
%CatHist = histcounts2(log10(CatDist(:)),CatTan(:).*RAD, LogHistDistEdges,InPar.HistRotEdges);    
% faster
CatHist = tools.array.hist2d_fast(log10(CatDist(:)),CatTan(:).*RAD, LogHistDistEdges,InPar.HistRotEdges);    

% make sure that CatHist doesn't contain zeros
Rep0Val = min((1-sum(CatHist==0,'all')./numel(CatHist)).^2,0.5);
CatHist(CatHist==0) = Rep0Val;
% normalize the histogram in the distance direction
CatHist = CatHist./mean(CatHist,2);



Nflip = size(InPar.Flip,1);
for Iflip=1:1:Nflip

    
    % Ref:
    RefX = Ref(:,InPar.RefColX).*InPar.Flip(Iflip,1);
    RefY = Ref(:,InPar.RefColY).*InPar.Flip(Iflip,2);
    % all possible differences between sources in Ref
    RefDiffX = RefX - RefX.';
    RefDiffY = RefY - RefY.';
    % all possible distances/angle between sources in Cat
    RefDist  = sqrt(RefDiffX.^2 + RefDiffY.^2);
    RefTan   = atan(RefDiffY./RefDiffX);
    % dilute the catalog (speed)
    %Fr = abs(RefDiffX(:))<MaxDist & abs(RefDiffY(:))<MaxDist;
    Fr = RefDist<MaxDist;
    RefDist = RefDist(Fr);
    RefTan  = RefTan(Fr);
    % histogram
    RefHist = histcounts2(log10(RefDist(:)),RefTan(:).*RAD, LogHistDistEdges, InPar.HistRotEdges);
    % make sure that RefHist doesn't contain zeros
    Rep0Val = min((1-sum(RefHist==0,'all')./numel(RefHist)).^2,0.5);
    RefHist(RefHist==0) = Rep0Val;
    % normalize the histogram in the distance direction
    RefHist = RefHist./mean(RefHist,2);

    
    
    % cross correlate the two histograms
    CC  = (ifft2(fft2((CatHist)).*conj(fft2((RefHist)))));
    
    % normalize by the noise
    %CCn = (CC - mean(CC,2))./std(CC,[],2);
    CCn = (CC - mean(CC,'all'))./std(CC,[],'all');
    CCn = fftshift(CCn,2);
    
    % search for local maxuma
    %[BW,Pos,MaxIsn] = imUtil.image.local_maxima(CCn,1,InPar.Threshold,InPar.Conn);
    % much faster:
    [Pos] = imUtil.sources.findLocalMax(CCn, 'Variance',1, 'Threshold',InPar.Threshold, 'Conn',InPar.Conn, 'Algo','findlocal');
    
    % search for peaks which are in the upper part of the correlation map
    % this peaks have scale which is smaller than 1
    Nsc = numel(LogHistDistEdges);
    FlagUpper = Pos(:,2)>(Nsc.*0.5);
    PosU2 = Nsc - Pos(:,2);
    
    Rot = InPar.HistRotEdges(Pos(:,1)); % - min(InPar.HistRotEdges);
    
    Scale     = 10.^(LogHistDistEdges(Pos(:,2))-LogHistDistEdges(1));
    ScaleUp   = 10.^(-(LogHistDistEdges(PosU2+1)-LogHistDistEdges(1)));
    Scale(FlagUpper) = ScaleUp(FlagUpper);    
    
    Collect(Iflip).Scale = Scale(:).';
    Collect(Iflip).Rot   = Rot(:).';
    Collect(Iflip).SN    = Pos(:,3).';
    Collect(Iflip).Flip  = InPar.Flip(Iflip,:).'.*ones(1,numel(Scale));
    
end

SN    = [Collect.SN].';
Rot   = [Collect.Rot].';
Scale = [Collect.Scale].';
Flip  = [Collect.Flip].';

Flag = Scale>InPar.Scale(1) & Scale<InPar.Scale(2);

Res.SN    = SN(Flag);
Res.Rot   = Rot(Flag);
Res.Scale = Scale(Flag);
Res.Flip  = Flip(Flag,:);

Flag = imUtil.patternMatch.select_maxima(SN,'MaxMethod',InPar.MaxMethod,'Threshold',InPar.Threshold,'FracOfMax',InPar.FracOfMax);

Res.SN    = SN(Flag);
Res.Rot   = Rot(Flag);
Res.Scale = Scale(Flag);
Res.Flip  = Flip(Flag,:);


    
    