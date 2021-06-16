function [Param,Res,ResLoop,Tran]=fit_astrometric_tran(Cat,Ref,varargin)
% Fit astrometric transformation
% Package: +imUtil.patternMatch
% Description: Fit astrometruc transformation to two matched catalogs.
%              Xref = a_0 + a_1*Xcat + a_2*Ycat + ...
%                         + b_0.*Ccat + b_1*Xcat*Ccat + b_2*Ycat*Ccat + ...
% Input  : - Matrix containing catalog with [X,Y,[Mag]].
%          - Reference matrix catalog with at least 2 columns.
%            Mandatory columns are [X,Y] and optional columns are
%            [Mag, Color, AirMass, ParallacticAngle, ModulusXpix,
%            ModulusYpix].
%          * Pairs of ...,key,val,... arguments.
%            The possible keywords are:
%            'FunX'
%            'FunY'
%            'FunNX'
%            'NunNY'
%            'Tran'
%            'MaxIter'
%            'ErrPos' - 
%            'Norm'
%            'FitMethod'
%            'Algo'
%            'ColCatX'
%            'ColCatY'
%            'ColCatM'
%            'ColRefX'
%            'ColRefY'
%            'ColRefM'
%            'ColRefC'
%            'ColRefAM'
%            'ColRefPA'
%            'ColRefModX'
%            'ColRefModY'
%---- Parameters of resid_vs_mag  ----
%            'BinMethod' - Method to use:
%                   'poly' - polynomial fit.
%                   'bin' - binning the data.
%                   Default is 'bin'
%            'PolyDeg' - Polynomial degree for the polynomial fit.
%                   Default is 3.
%            'BinSize' - Bin size for binning. Default is 1 (mag).
%            'FunMean' - A function handle to use when calculating the mean
%                   of the data in each bin.
%                   Default is @nanmedian.
%            'FunStd' - A function handle to use when calculating the std
%                   of the data in each bin, or when calculating the global
%                   std after the polynomial fit.
%                   Default is @imUttil.background.rstd.
%            'InterpMethod' - Interpolation method. Default is 'linear'.
%            'ThresholdSigma' - Threshold in sigmas (std) for flagging good
%                   data. Default is 3.
% Output : - 
% Example:
% [Param,Res,ResLoop]=imUtil.patternMatch.fit_astrometric_tran;

if nargin==0
    % simulation mode
    
    Nsrc = 1000;
    Cat = rand(Nsrc,3).*[1024 1024 10];
    Ref = Cat + 0.1.*randn(Nsrc,3);
    Ref = [Ref, rand(Nsrc,1).*2];
    
end



InPar = inputParser;
% addOptional(InPar,'FunX',{@(x,y,c,AM,PA) ones(size(x)),...
%                           @(x,y,c,AM,PA) x,...
%                           @(x,y,c,AM,PA) y,...
%                           @(x,y,c,AM,PA) 2.*x.^2-1,...
%                           @(x,y,c,AM,PA) 2.*y.^2-1,...
%                           @(x,y,c,AM,PA) x.*y,...
%                           @(x,y,c,AM,PA) 4.*x.^3 - 3.*x,...
%                           @(x,y,c,AM,PA) 4.*y.^3 - 3.*y,...
%                           @(x,y,c,AM,PA) (2.*x.^2-1).*y,...
%                           @(x,y,c,AM,PA) (2.*y.^2-1).*x,...
%                           @(x,y,c,AM,PA) c,...
%                           @(x,y,c,AM,PA) x.*c,...
%                           @(x,y,c,AM,PA) y.*c});
% addOptional(InPar,'FunY',{@(x,y,c,AM,PA) ones(size(x)),...
%                           @(x,y,c,AM,PA) x,...
%                           @(x,y,c,AM,PA) y,...
%                           @(x,y,c,AM,PA) 2.*x.^2-1,...
%                           @(x,y,c,AM,PA) 2.*y.^2-1,...
%                           @(x,y,c,AM,PA) x.*y,...
%                           @(x,y,c,AM,PA) 4.*x.^3 - 3.*x,...
%                           @(x,y,c,AM,PA) 4.*y.^3 - 3.*y,...
%                           @(x,y,c,AM,PA) (2.*x.^2-1).*y,...
%                           @(x,y,c,AM,PA) (2.*y.^2-1).*x,...
%                           @(x,y,c,AM,PA) c,...
%                           @(x,y,c,AM,PA) x.*c,...
%                           @(x,y,c,AM,PA) y.*c});
                      
addOptional(InPar,'FunX',{@(x,y,c,AM,PA) ones(size(x)),...
                          @(x,y,c,AM,PA) x,...
                          @(x,y,c,AM,PA) y,...
                          @(x,y,c,AM,PA) 2.*x.^2-1,...
                          @(x,y,c,AM,PA) 2.*y.^2-1,...
                          @(x,y,c,AM,PA) x.*y});
                          
addOptional(InPar,'FunY',{@(x,y,c,AM,PA) ones(size(x)),...
                          @(x,y,c,AM,PA) x,...
                          @(x,y,c,AM,PA) y,...
                          @(x,y,c,AM,PA) 2.*x.^2-1,...
                          @(x,y,c,AM,PA) 2.*y.^2-1,...
                          @(x,y,c,AM,PA) x.*y});
                      
addOptional(InPar,'FunNX',@(x,nx1,nx2) (x-nx1)./nx2);
addOptional(InPar,'FunNY',@(y,ny1,ny2) (y-ny1)./ny2);

addOptional(InPar,'Tran',[]);  % optional tran2dCl object - override Fun...
addOptional(InPar,'MaxIter',2);
addOptional(InPar,'ErrPos',1e-5);  % total error in one axis - all contributions
addOptional(InPar,'Norm',NaN);  % empty - do nothing, NaN - auto, or [centerX, RangeX, centerY, RangeY]
addOptional(InPar,'FitMethod','lscov'); % 'lscov' | '\'
addOptional(InPar,'Algo','chol'); % 'orth' | 'chol'
addOptional(InPar,'ColCatX',1);
addOptional(InPar,'ColCatY',2);
addOptional(InPar,'ColCatM',3);
addOptional(InPar,'ColRefX',1);
addOptional(InPar,'ColRefY',2);
addOptional(InPar,'ColRefM',3);
addOptional(InPar,'ColRefC',4);
addOptional(InPar,'ColRefAM',[]);
addOptional(InPar,'ColRefPA',[]);
addOptional(InPar,'ColRefModX',[]);
addOptional(InPar,'ColRefModY',[]);
% resid_vs_mag arguments
addOptional(InPar,'MagRange',[]);  % empty - no limit
addOptional(InPar,'MaxResid',1);  % pixels
addOptional(InPar,'BinMethod','bin'); % 'bin' | 'poly'
addOptional(InPar,'PolyDeg',3);
addOptional(InPar,'BinSize',1);
addOptional(InPar,'FunMean',@nanmedian);
addOptional(InPar,'FunStd',@imUtil.background.rstd);
addOptional(InPar,'InterpMethod','linear');
addOptional(InPar,'ThresholdSigma',3);

parse(InPar,varargin{:});
InPar = InPar.Results;

if size(Cat,1)~=size(Ref,1)
    error('Cat and Ref maust be matched catalogs with the same number of lines');
end

% clean Cat and Ref from NaN's
FlagGood = ~isnan(sum(Cat,2)) & ~isnan(sum(Ref,2));
Cat      = Cat(FlagGood,:);
Ref      = Ref(FlagGood,:);


Nsrc = size(Ref,1);

if Nsrc~=size(Cat,1)
    error('Number of sources in Ref and Cat must be identical');
end

InPar.ErrPos = InPar.ErrPos.*ones(Nsrc,1);

RefX = Ref(:,InPar.ColRefX);
RefY = Ref(:,InPar.ColRefY);

% Ref mag
if isempty(InPar.ColRefM)
    RefMag = zeros(Nsrc,0);
else
    RefMag = Ref(:,InPar.ColRefM);
end
% Ref color
if isempty(InPar.ColRefC)
    RefColor = zeros(Nsrc,1);
else
    RefColor = Ref(:,InPar.ColRefC);
end
% Ref airmass
if isempty(InPar.ColRefAM)
    RefAM = zeros(Nsrc,1);
else
    RefAM = Ref(:,InPar.ColRefAM);
end
% Ref parallactic angle
if isempty(InPar.ColRefPA)
    RefPA = zeros(Nsrc,1);
else
    RefPA = Ref(:,InPar.ColRefPA);
end
% Ref modulus X pix
if isempty(InPar.ColRefModX)
    RefModX = zeros(Nsrc,1);
else
    RefModX = Ref(:,InPar.ColRefModX);
end
% Ref modulus Y pix
if isempty(InPar.ColRefModY)
    RefModY = zeros(Nsrc,1);
else
    RefModY = Ref(:,InPar.ColRefModY);
end





CatX   = Cat(:,InPar.ColCatX);
CatY   = Cat(:,InPar.ColCatY);
%CatMag = Cat(:,InPar.ColCatM);


% renormalize coordinate system including errors
if isempty(InPar.Norm)
    % no normalization to Ref coordinates
    CenterX = 0;
    CenterY = 0;
    RangeX  = 1;
    RangeY  = 1;
        
else
    if isnan(InPar.Norm)
        % auto normalization
        MaxX = max(RefX);
        MinX = min(RefX);
        MaxY = max(RefY);
        MinY = min(RefY);
        RangeX = MaxX - MinX;
        RangeY = MaxY - MinY;
        CenterX = (MaxX+MinX).*0.5;
        CenterY = (MaxY+MinY).*0.5;
        
        RefX = (RefX - CenterX)./(RangeX.*0.5);
        RefY = (RefY - CenterY)./(RangeY.*0.5);
    else
        % use specified normaliaztion
        CenterX = InPar.Norm(1);
        CenterY = InPar.Norm(3);
        RangeX  = InPar.Norm(2);
        RangeY  = InPar.Norm(4);
    end
end
Param.FunNX = InPar.FunNX;
Param.FunNY = InPar.FunNY;
Param.ParNX   = [CenterX, RangeX.*0.5];
Param.ParNY   = [CenterY, RangeY.*0.5];
   
if ~isempty(InPar.Tran)
    % tran2dCl object is provided
    
    Tran = InPar.Tran;
    Tran.ParNX = Param.ParNX;
    Tran.ParNY = Param.ParNY;
    
    NparX = numel(Tran.FunX);
    NparY = numel(Tran.FunY);
    
    [Hx,Hy] = design_matrix(Tran,[RefX,RefY,RefColor,RefAM,RefPA]);
    
else

    % construct design matrix for position
    NparX = numel(InPar.FunX);
    Hx    = zeros(Nsrc,NparX);
    for Ix=1:1:NparX
        Hx(:,Ix) = InPar.FunX{Ix}(RefX,RefY,RefColor,RefAM,RefPA);  
    end

    NparY = numel(InPar.FunY);
    Hy    = zeros(Nsrc,NparY);
    for Iy=1:1:NparY
        Hy(:,Iy) = InPar.FunY{Iy}(RefX,RefY,RefColor,RefAM,RefPA);
    end
end


% fitting
Iter = 0;
% fit all sources in first iteration
FlagSrc = true(Nsrc,1);

% formal error only
Var     = InPar.ErrPos.^2;
% error including additional contributions (e.g., scintilations)
InvVar  = 1./Var;
ResResid = [];
while Iter<InPar.MaxIter
    %
    Iter = Iter + 1;
    
    switch lower(InPar.FitMethod)
        case 'lscov'
            [ParX,ParErrX] = lscov(Hx(FlagSrc,:), CatX(FlagSrc), InvVar(FlagSrc), InPar.Algo);
            
            [ParY,ParErrY] = lscov(Hy(FlagSrc,:), CatY(FlagSrc), InvVar(FlagSrc), InPar.Algo);
            
        case '\'
            ParX = Hx(FlagSrc,:)\CatX(FlagSrc);
            ParErrX = nan(size(ParX));
            
            ParY = Hy(FlagSrc,:)\CatY(FlagSrc);
            ParErrY = nan(size(ParY));
            
        otherwise
            error('Unknwon FitMethod option');
    end
        
    % calculate the residuals and rms of the fit
    ResidX = CatX - Hx*ParX;
    ResidY = CatY - Hy*ParY;
    Resid  = sqrt(ResidX.^2 + ResidY.^2);
    % RMS is calculated only for selected sources
    RMS_X  = std(ResidX(FlagSrc));
    RMS_Y  = std(ResidY(FlagSrc));
    RMS    = sqrt(RMS_X.^2 + RMS_Y.^2);
    
    % screening of sources
    if Iter<InPar.MaxIter
        % select good sources and re-estimate positional error
        
        % calculate RMS vs. mag.
        [FlagSrc,ResResid] = imUtil.calib.resid_vs_mag(RefMag(FlagSrc),Resid(FlagSrc),...
                                                              'MagRange',InPar.MagRange,...
                                                              'BinMethod',InPar.BinMethod,...
                                                              'PolyDeg',InPar.PolyDeg,...
                                                              'BinSize',InPar.BinSize,...
                                                              'FunMean',InPar.FunMean,...
                                                              'FunStd',InPar.FunStd,...
                                                              'InterpMethod',InPar.InterpMethod,...
                                                              'ThresholdSigma',InPar.ThresholdSigma);
        
        % Applay MagRnage
        if ~isempty(InPar.MagRange)
            FlagSrc = FlagSrc & RefMag>InPar.MagRange(1) & RefMag<InPar.MagRange(2);
        end
        % apply - removing sources with large residuals
        FlagSrc = FlagSrc & Resid<InPar.MaxResid;
        
        % add error as a function of mag to basic error
        InvVar = 1./(Var + ResResid.InterpMeanResid.^2);
        
    end
    
    ResLoop(Iter).Resid = Resid;
    ResLoop(Iter).RMS_X = RMS_X;
    ResLoop(Iter).RMS_Y = RMS_Y;
    ResLoop(Iter).RMS   = RMS;
    ResLoop(Iter).Flag  = FlagSrc;
end

[~,ResResid] = imUtil.calib.resid_vs_mag(RefMag(FlagSrc),Resid(FlagSrc),...
                                                              'MagRange',InPar.MagRange,...
                                                              'BinMethod',InPar.BinMethod,...
                                                              'PolyDeg',InPar.PolyDeg,...
                                                              'BinSize',InPar.BinSize,...
                                                              'FunMean',InPar.FunMean,...
                                                              'FunStd',InPar.FunStd,...
                                                              'InterpMethod',InPar.InterpMethod,...
                                                              'ThresholdSigma',InPar.ThresholdSigma);

Param.FunX  = InPar.FunX;
Param.FunY  = InPar.FunY;

Param.ParX     = ParX;
Param.ParY     = ParY;
Param.ParErrX  = ParErrX;
Param.ParErrY  = ParErrY;

Res.ResResid = ResResid;
Res.Resid    = Resid;
Res.ResidX   = ResidX;
Res.ResidY   = ResidY;
Res.FlagSrc  = FlagSrc;
Res.Ngood    = sum(FlagSrc);
Res.Resid    = Resid;
Res.RefMag   = RefMag;
Res.RMS_X    = RMS_X;
Res.RMS_Y    = RMS_Y;
Res.RMS      = RMS;
Res.CatX     = CatX;
Res.CatY     = CatY;
Res.RefX     = RefX;
Res.RefY     = RefY;



if isempty(ResResid)
    % no asymptotic rms
    Res.AssymRMS     = NaN;
    Res.AssymRMS_mag = NaN;
    Res.AssymRMS_RMS = NaN;
else
    TmpMag = ResResid.InterpMeanResid; %(Res.FlagSrc);
    TmpStd = ResResid.InterpStdResid;  %(Res.FlagSrc);
    [MinMeanRMS, MinMeanInd] = min(TmpMag);
    Res.AssymRMS     = MinMeanRMS;
    Res.AssymRMS_mag = TmpMag(MinMeanInd);
    Res.AssymRMS_RMS = TmpStd(MinMeanInd);
end
Res.ResResid = ResResid;


% populate a tran2dCl object
if nargout>3
    if isempty(InPar.Tran)
        Tran = tran2dCl;
        Tran.FunX  = Param.FunX;
        Tran.FunY  = Param.FunY;
        Tran.FunNX = Param.FunNX;
        Tran.FunNY = Param.FunNY;
        Tran.ParX  = Param.ParX;
        Tran.ParY  = Param.ParY;
        Tran.ParNX = Param.ParNX;
        Tran.ParNY = Param.ParNY;
    else
        Tran.ParX  = Param.ParX;
        Tran.ParY  = Param.ParY;
    end
end


