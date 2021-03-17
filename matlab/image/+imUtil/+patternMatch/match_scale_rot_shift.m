function [Sol,PrevStep,Matched]=match_scale_rot_shift(Cat,Ref,varargin)
% Affine transformation matching of the coordinate systems of two catalogs
% Package: imUtil.patternMatch
% Description: Given two catalogs that coordinadte systems are related
%              by flip, scale, rotation and shift, search the the approximate
%              affine transformation
%              that is required in order to align the coordinate systems of
%              the two catalogs. The search is done by matching patterns in
%              one catalog to the other.
% Input  : - A catalog that contains Xcat,Ycat coordinates.
%            Alternatively, if only one argument is sprovided then will run
%            in simulation mode.
%          - A referece catalog that contains Xref,Yref coordinates.
%            Xcat/Ycat and Xref/Yref are related by flip, scaling, rotation 
%            and shift.
%          * Pairs of ...,key,val,... Possible keywords include:
%            'Scale' - The scale to apply to the reference catalog in order
%                   to convert it to the input catalog. If this is a
%                   scalar, then will not attempt to search for the best
%                   scaleing. If a two element vector, then these are the
%                   [min, max] scale range to search.
%            'HistDistEdgesRotScale' - [MinDist, MaxDist, NumberOf points] for
%                     distance histogram.
%                     Default is [10 600 300].
%            'HistRotEdges' - Edges for angle axis of the histogram.
%                     If a scalar is provided then this rotation will be
%                     assumed.
%                     Default is (-90:0.2:90).'.
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
%            'Flip' - A two column matrix of all possible flips to test.
%                     Use 'all' to check all flips - i.e., [1 1; 1 -1;-1 -1; -1 1].
%                     Default is [1 1].
%            'SearchRadius' - Searchj radius for final source matching
%                     [pix]. Default is 4.
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
%                     Default is @nanmedian.
%            'BackFunPar' - Used for 'sn' PeakMethod.
%                     For details see imUtil.background.background.
%                     Default is {'all'}.
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
% Output : - A structure of possible solutions for matching between the two
%            catalogs. Follwoing fields are available:
%            .SN
%            .MaxVal
%            .Flip
%            .Rot
%            .Scale
%            .ShiftX - The shift in X one need to add to Ref in order to
%                   get Cat.
%            .ShiftY - The shift in Y one need to add to Ref in order to
%                   get Cat.
%            .AffineTran - A cell array of affine matrix transformations
%               Apply this transformation to Ref using imUtil.cat.affine2d_transformation
%               In order to get Cat.
%               This is the rotation transformation for the reference frame
%               and not the reference coordinates.
%          - A structure of matching steps, including ResShift and ResRot.
%            ResShift is documented in
%            imUtil.patternMatch.find_shift_pairs, while ResRot in imUtil.patternMatch.find_rot_pairs
%            or imUtil.patternMatch.find_scalerot_pairs.
%          - A structure containing the matched sources for each solution.
%            The following fields are available:
%            'MatchedCat - [X,Y] of the sources in Cat matched to 
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: [Sol,PrevStep,Matched]=imUtil.patternMatch.match_scale_rot_shift
% Reliable: 


if (nargin<2)
    % simulation mode
    if nargin<1
        Cat = 3;
    end
    SimMode = Cat;
    
    switch SimMode
        case 1
            % only shift
            

            % simulation mode
            Nstar = 1000;
            Ref = rand(Nstar,2).*2048 - 1024;
            %Ref = sortrows(Ref,2);
            Noverlap = 300;
            Cat = [Ref(1:Noverlap,1), Ref(1:Noverlap,2)];
            Cat = [Cat; rand(Nstar-Noverlap,2).*1024];
            Cat(:,1) = Cat(:,1) + 220 + randn(Nstar,1).*0.3;
            Cat(:,2) = Cat(:,2) + 130 + randn(Nstar,1).*0.3;
            Cat      = sortrows(Cat,2);
            Ref      = Ref.*[1 -1];

        case 2
            % simulation mode
            Ns = 5000;
            Nm = 2000;
            Theta = 35;

            Ref = rand(Ns,2).*2048 - 1024;
            Cat = Ref(1:Nm,:) + rand(Nm,2).*0.1;
            Cat = [Cat; rand(Nm,2).*1024];

            % [Theta, Sclae, ShiftX, ShiftY, FlipX, FlipY];
            %Tr = [35, 1, 120, 45, 1, 1];
            Tr = [35, 1, 120, 45, 1, -1];
            [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,Tr,'+');
            Cat = [NewX, NewY];

                
        case 3
            % Shift rotation
  
            % simulation mode
            Ns = 5000;
            Nm = 2000;
 
            Ref = rand(Ns,2).*2048 - 1024;
            Cat = Ref(1:Nm,:) + rand(Nm,2).*0.1;
            

            %Tr = [-15, 1.0, -20, 45, 1, 1];  % ok
            %Tr = [-115, 1.0, -20, 45, 1, 1];  % ok
            %Tr = [15, 1.0, -20, 45, 1, 1];   % ok
            %Tr = [115, 1.0, -20, 45, 1, 1];  % ok
            %Tr = [-15, 1.0, -20, 45, -1, -1];  % ok
            Tr = [-115, 1.0, -20, 45, -1, -1];  % ok
            %Tr = [15, 1.0, -20, 45, -1, -1];  % ok
            %Tr = [115, 1.0, -20, 45, -1, -1];  % ok
            %Tr = [15, 1.0, -20, 45, 1, -1];  % ok
            %Tr = [115, 1.0, -20, 45, 1, -1];  % ok
            %Tr = [-115, 1.0, -20, 45, 1, -1];  % ok
            %Tr = [-15, 1.1, -20, 45, 1, -1];  % ok
            
            
            [NewX,NewY]=imUtil.cat.affine2d_transformation(Cat,Tr,'+');
            Cat = [NewX, NewY];
            
            Cat = [Cat; rand(Nm,2).*1024];
            
            % Rotate Ref
            %Theta = -10;
%             Rot = [cosd(Theta), -sind(Theta); sind(Theta), cosd(Theta)];
% 
%             Scale = 1.1;
%             Cat = Scale.*(Rot*Cat.').' +[25 -134];
% 
%             Cat = Cat.*[1 1];
        otherwise
            error('Unknown SimMode option');
    end
end



InPar = inputParser;

addOptional(InPar,'Scale',[1.0]); % scale or [min max] range that require to ?

addOptional(InPar,'HistDistEdgesRotScale',[10 600 300]);
addOptional(InPar,'HistDistEdgesRot',(12:3:300).');

addOptional(InPar,'HistRotEdges',(-90:0.2:90));  % rotation or [min max] rotation that require to ?
addOptional(InPar,'RangeX',[-1000 1000]); 
addOptional(InPar,'RangeY',[-1000 1000]); 
addOptional(InPar,'StepX',4); 
addOptional(InPar,'StepY',4); 
addOptional(InPar,'Flip',[1 1; 1 -1;-1 1;-1 -1]); 
addOptional(InPar,'SearchRadius',4); 

% maxima finding
addOptional(InPar,'MaxMethod','thresh_fracmax');
addOptional(InPar,'Threshold',5);
addOptional(InPar,'FracOfMax',0.8);
addOptional(InPar,'Conn',8);

% background pars for find_shift_pairs
addOptional(InPar,'BackFun',@nanmedian); % @median);
addOptional(InPar,'BackFunPar',{'all'});      % {[1 2],'omitnan'});
addOptional(InPar,'VarFun',@imUtil.background.rvar);    % if empty, then will try to read var from second output of BackFun...
addOptional(InPar,'VarFunPar',{}); % {[1 2]});
addOptional(InPar,'SubSizeXY',[128 128]);  % or 'full'
addOptional(InPar,'Overlap',[16]); 
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


% Ref catalog with [X,Y]
RefXY = Ref(:,[InPar.RefColX, InPar.RefColY]);
Nflip    = numel(InPar.Flip);

if numel(InPar.Scale)>1
    % find scale and rotation
    Res = imUtil.patternMatch.find_scalerot_pairs(Cat,RefXY,'CatColX',InPar.CatColX,...
                                                     'CatColY',InPar.CatColY,...
                                                     'RefColX',1,...
                                                     'RefColY',2,...
                                                     'Flip',InPar.Flip,...
                                                     'HistRotEdges',InPar.HistRotEdges,...
                                                     'HistDistEdges',InPar.HistDistEdgesRotScale,...
                                                     'MaxMethod',InPar.MaxMethod,...
                                                     'Threshold',InPar.Threshold,...
                                                     'FracOfMax',InPar.FracOfMax,...
                                                     'Conn',InPar.Conn,...
                                                     'Scale',InPar.Scale);
                                                     
else
    % assume scale is known
    
    if numel(InPar.HistRotEdges)>1
        % find rotation
        % applay Scale to Refeence
        
        % No need to loop over all flips because find_rot_pairs will do it
        
        %Z0 = zeros(size(InPar.Scale));
        %[RefNewX,RefNewY]=imUtil.cat.affine2d_transformation(Ref,[0, InPar.Scale, 0 0 InPar.Flip]);
        % rotate the reference frame:
        [RefNewX,RefNewY]=imUtil.cat.affine2d_transformation(RefXY,[0, InPar.Scale, 0 0 1 1],'+');
        Scaled = true;
        % RefNew12 now is also scaled
        RefScaledXY = [RefNewX, RefNewY];
        
        % note the catalog was not fliped
        % note the catalog was scaled
        Res = imUtil.patternMatch.find_rot_pairs(Cat,RefScaledXY,'CatColX',InPar.CatColX,...
                                                         'CatColY',InPar.CatColY,...
                                                         'RefColX',1,...
                                                         'RefColY',2,...
                                                         'Flip',InPar.Flip,...
                                                         'HistRotEdges',InPar.HistRotEdges,...
                                                         'HistDistEdges',InPar.HistDistEdgesRot,...
                                                         'MaxMethod',InPar.MaxMethod,...
                                                         'Threshold',InPar.Threshold,...
                                                         'FracOfMax',InPar.FracOfMax);
                                                     %'Flip',[1 1],...
        Ns = numel(Res.SN);
        Res.Scale = InPar.Scale.*ones(Ns,1);
    else
        % assume both rotation and scale are known.
        Res.SN    = NaN;
        Res.Rot   = InPar.HistRotEdges;
        Res.Scale = InPar.Scale;
        Res.Flip  = InPar.Flip;
    end
end


AllSol = zeros(0,11);
if ~isempty(Res.Flip)
    % go over all selected solutins
    Nsol = numel(Res.Rot);
    
    
    for Isol=1:1:Nsol

        [RefNewX,RefNewY]=imUtil.cat.affine2d_transformation(RefXY,[Res.Rot(Isol), Res.Scale(Isol), 0 0 Res.Flip(Isol,:)],'+');
        
        % rotate the reference frame
        %[RefNewX,RefNewY]=imUtil.cat.affine2d_transformation(RefNew12,[Res.Rot(Isol), Res.Scale(Isol), 0 0 1 1],'+');
                
        RefXYforShift = [RefNewX, RefNewY];

        % assume both scale and rotation are known
        % the coordinates are already fliped so use [1 1]
        ResS(Isol) = imUtil.patternMatch.find_shift_pairs(Cat,RefXYforShift,'CatColX',InPar.CatColX,...
                                                            'CatColY',InPar.CatColY,...
                                                            'RefColX',1,...
                                                            'RefColY',2,...
                                                            'Flip',[1 1],...
                                                            'RangeX',InPar.RangeX,...
                                                            'RangeY',InPar.RangeY,...
                                                            'StepX',InPar.StepX,...
                                                            'StepY',InPar.StepY,...
                                                            'MaxMethod',InPar.MaxMethod,...
                                                            'Threshold',InPar.Threshold,...
                                                            'FracOfMax',InPar.FracOfMax,...
                                                            'Conn',InPar.Conn,...
                                                            'BackFun',InPar.BackFun,...
                                                            'BackFunPar',InPar.BackFunPar,...
                                                            'VarFun',InPar.VarFun,...
                                                            'VarFunPar',InPar.VarFunPar,...
                                                            'SubSizeXY',InPar.SubSizeXY,...
                                                            'Overlap',InPar.Overlap,...
                                                            'MinVariance',InPar.MinVariance);
                                                        %'Flip',[1 1],...
                                                        %Res.Flip(Isol,:),...
                                                        % 'Flip',Res.Flip(Isol,:),...

         Ns = numel(ResS(Isol).SN);
         
         % update ResS based on the input flip
         ResS(Isol).Flip = Res.Flip(Isol,:).*ones(Ns,1);
         
         %                       1        2         3        4 5                       6                   7
         AllSol = [AllSol; [[Res.SN(Isol), Res.Rot(Isol), Res.Scale(Isol), Res.Flip(Isol,:)].*ones(Ns,1), ...
                   ResS(Isol).ShiftX, ResS(Isol).ShiftY, ResS(Isol).MaxVal, ResS(Isol).SN, ResS(Isol).Flip]];
         
    end    
                                                            
else
    ResS = [];
end
Flag = imUtil.patternMatch.select_maxima(AllSol(:,9),'MaxMethod',InPar.MaxMethod,...
                                 'Threshold',InPar.Threshold,...
                                 'FracOfMax',InPar.FracOfMax);

Sol.SN     = AllSol(Flag,9);
Sol.MaxVal = AllSol(Flag,8);
Sol.Flip   = AllSol(Flag,4:5);
Sol.Rot    = AllSol(Flag,2);
Sol.Scale  = AllSol(Flag,3);
Sol.ShiftX = AllSol(Flag,6);
Sol.ShiftY = AllSol(Flag,7);

PrevStep.ResRot   = Res;
PrevStep.ResShift = ResS;


for I=1:1:numel(Sol.SN)
    % note that this is the transformation for coordinates rotation (not
    % reference frame rotation).
    [~,~,Sol.AffineTran{I}] = imUtil.cat.affine2d_transformation([],[Sol.Rot(I), Sol.Scale(I), Sol.ShiftX(I), Sol.ShiftY(I), Sol.Flip(I,1), Sol.Flip(I,2)],'+');
end
    
    
if nargout>2
    Matched = [];
    for I=1:1:numel(Sol.SN)
        % applay transformation to Ref
        %[Theta, Scale, ShiftX, ShiftY, FlipX, FlipY]
        %AffinePar = 
        %[Sol.Rot(I), Sol.Scale(I), Sol.ShiftX(I), Sol.ShiftY(I),  Sol.Flip(I,:)]
        AffinePar = Sol.AffineTran{I};
        %AffinePar = [15, 1, -20, 45, 1, 1]   % ok
        %AffinePar = [15, 1, -18, 46, 1, 1]   % ok


        [RefNewX,RefNewY]=imUtil.cat.affine2d_transformation(RefXY,AffinePar,'+');
        RefTranXY = [RefNewX,RefNewY];

        % match stars
        [Matched(I).ResM,Matched(I).MatchedCat,Matched(I).Ref] = imUtil.cat.match_sources_xy(Cat,RefTranXY,'SearchRadius',InPar.SearchRadius,...
                                                                    'IsCatSorted',false,...
                                                                    'CatColX',InPar.CatColX,...
                                                                    'CatColY',InPar.CatColY,...
                                                                    'RefColX',1,...
                                                                    'RefColY',2);
  

      
        
            
        %hist(ResMatch.DeltaX)

    end
end






