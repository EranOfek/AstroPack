function Res=find_rot_pairs(Cat,Ref,varargin)
% Find the best rotation required to match two catalogs using dist-rot hist
% Package: imUtil.patternMatch
% Description: Find the best rotation required to match two catalogs using
%              The Kaiser method. The histogram of the distance and
%              angle between stars in the reference is cross correlated
%              with the same histogram for the reference.
%              This function will return at least two solutions, where one
%              of the solutions is wrong.
% Input  : - A catalog.
%            If no arguments are provided, then run in simulation mode.
%          - A reference catalog.
%          * Pairs of ...,key,val,... The following keywords are avaialble:
%            'Flip' - A two column matrix of all possible flips to test.
%                     Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].
%                     Default is [1 1; 1 -1].
%                     This default covers all options.
%            'HistDistEdges' - Edges for distance axis of the histogram.
%                     Default is (12:3:300).'.
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
%                     Default is 4.
%            'FracOfMax' - The parameter that that used in '*_fracmax' and
%                     for selecting peaks.
%                     Only peaks that above the maximal peak multiplied by
%                     this parameter will be returned.
%            'CatColX' - Catalog column that contains the X axis. Default is 1.
%            'CatColY' - Catalog column that contains the Y axis. Default is 2.
%            'RefColX' - Reference column that contains the X axis. Default is 1.
%            'RefColY' - Reference column that contains the Y axis. Default is 2.
% Output : - A structure containing the following fields:
%            'SN' - A vector of the S/N of all selected
%                   cross-correkation peaks.
%            'Rot' - A vector of all rotation angles of all selected
%                   cross-correlation peaks [deg].
%                   This is is the angle by which one should rotate Ref, in
%                   order to get Cat.
%                   Apply this transformation using
%                   imUtil.cat.affine2d_transformation(Ref,Tr,'+');
%            'Flip' - A two column matrix [FlipX, FlipY] corresponding to
%                   the flips of all selected peaks. This is is the flip
%                   needed to be applied to Ref, in
%                   order to get Cat.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Res=imUtil.patternMatch.find_rot_pairs;
% Reliable: 2
%--------------------------------------------------------------------------

RAD = 180./pi;

if (nargin<2)
    % simulation mode
    if (nargin==0)
        Theta = 24.2;
    else
        Theta = Cat;
    end
    % simulation mode
    Ns = 5000;
    Nm = 2000;
    
    Ref = rand(Ns,2).*2048 - 1024;
    Cat = Ref(1:Nm,:) + rand(Nm,2).*0.1;
    %Cat = [Cat; rand(Nm,2).*1024];
    
    %Cat = Cat.*[-1 1];
    
    Tr = [-120, 1, -120, -45, -1, 1];
    [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,Tr,'+');
    Cat = [NewX, NewY];
    
    
    % Rotate Ref
    %Theta = -10;
    %Rot = [cosd(Theta), -sind(Theta); sind(Theta), cosd(Theta)];
    
    %Cat = (Rot*Cat.').';
end


InPar = inputParser;

addOptional(InPar,'Flip',[1 1; -1 -1; 1 -1;-1 1]); %;-1 -1; -1 1]);
addOptional(InPar,'HistDistEdges',(12:3:300).');
addOptional(InPar,'HistRotEdges',(-90:0.2:90).');

addOptional(InPar,'MaxMethod','thresh_fracmax');
addOptional(InPar,'Threshold',4);
addOptional(InPar,'FracOfMax',0.8);

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


ThetaShift = InPar.HistRotEdges(:).'; % - min(InPar.HistRotEdges);



%--- Cat ---
CatX = Cat(:,InPar.CatColX); %.*InPar.Flip(Iflip,1);
CatY = Cat(:,InPar.CatColY); %.*InPar.Flip(Iflip,2);

% all possible differences between sources in Cat
CatDiffX = CatX - CatX.';
CatDiffY = CatY - CatY.';


Fc = abs(CatDiffX(:))<MaxDist & abs(CatDiffY(:))<MaxDist;
CatDiffX = CatDiffX(Fc);
CatDiffY = CatDiffY(Fc);

% all possible distances/angle between sources in Cat
CatDist  = sqrt(CatDiffX.^2 + CatDiffY.^2);
CatTan   = atan(CatDiffY./CatDiffX);
%CatTan   = atan2(CatDiffY,CatDiffX);
% calc histograms
% slower
%CatHist = histcounts2((CatDist(:)), CatTan(:).*RAD, InPar.HistDistEdges, InPar.HistRotEdges);
CatHist = tools.array.hist2d_fast((CatDist(:)), CatTan(:).*RAD, InPar.HistDistEdges, InPar.HistRotEdges);




Nflip = size(InPar.Flip,1);
for Iflip=1:1:Nflip

    %--- Ref ---
    RefX = Ref(:,InPar.RefColX).*InPar.Flip(Iflip,1);
    RefY = Ref(:,InPar.RefColY).*InPar.Flip(Iflip,2);
    % all possible differences between sources in Ref
    RefDiffX = RefX - RefX.';
    RefDiffY = RefY - RefY.';

    % all possible distances/angle between sources in Cat
    RefDist  = sqrt(RefDiffX.^2 + RefDiffY.^2);
    RefTan   = atan(RefDiffY./RefDiffX);
    %RefTan   = atan2(RefDiffY,RefDiffX);

    % dilute the catalog (speed)
    %Fr = abs(RefDiffX(:))<MaxDist & abs(RefDiffY(:))<MaxDist;
    Fr = RefDist<MaxDist;
    RefDist = RefDist(Fr);
    RefTan  = RefTan(Fr);
    % calc hist
    RefHist = histcounts2((RefDist(:)), RefTan(:).*RAD, InPar.HistDistEdges, InPar.HistRotEdges);
    
    

    % cross correlated
    CC  = (ifft2(fft2((CatHist)).*conj(fft2((RefHist)))));
    % normalize by the noise
    CCn = (CC - mean(CC,2))./std(CC,[],2);
    
    % take the first line that correspomds to matched scale
    
    CC1 = CCn(1,:);
    CC1 = fftshift(CC1);

    % [ListExt] = Util.find.find_local_extremum(InPar.HistRotEdges(:),CC1(:))
    % I = find(ListExt(:,3)<0 & ListExt(:,2)>InPar.Thresh)
    % ListExt(I,:)

    % pad CC1 with zeros to enable finding local max at edges
    FlagMax = islocalmax([0, CC1, 0]);
    FlagMax = FlagMax(2:1:end-1);
    Collect(Iflip).CC   = CC1(FlagMax);
    Collect(Iflip).Rot  = ThetaShift(FlagMax);
    Collect(Iflip).Flip = InPar.Flip(Iflip,:).'.*ones(1,sum(FlagMax));
    
%     if prod(Collect(Iflip).Flip)<0
%         Collect(Iflip).Rot = 180 - Collect(Iflip).Rot;
%     end
end

CC   = [Collect.CC].';
Rot  = [Collect.Rot].';
Flip = [Collect.Flip].';

Flag = imUtil.patternMatch.select_maxima(CC,'MaxMethod',InPar.MaxMethod,'Threshold',InPar.Threshold,'FracOfMax',InPar.FracOfMax);

Res.SN    = CC(Flag);
Res.Rot   = Rot(Flag);
Res.Flip  = Flip(Flag,:);


    
    