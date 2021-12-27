function Res=fit_astrometry(Catalog,varargin)
% 
% Package: imUtil.patternMatch
% Description: 
% Input  : - 
%          * Pairs of ...,key,val,... Possible keywords include:
%          
% Output : - A vector of logicals indicating the selected peaks.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Res=imUtil.patternMatch.fit_astrometry;
% Reliable: 2

RAD = 180./pi;
ARCSEC_DEG = 3600;

InPar = inputParser;

addOptional(InPar,'RA',[]);
addOptional(InPar,'Dec',[]);
addOptional(InPar,'CooUnits','deg');
addOptional(InPar,'Scale',1);  % arcsec/pix
addOptional(InPar,'Rot',(-10:0.2:10));
addOptional(InPar,'Flip',[1 -1]);
%addOptional(InPar,'Cat','GAIADR2'); 
addOptional(InPar,'Size',[0.5 0.5]);
addOptional(InPar,'SizeCatXY',[]); % in pixels

addOptional(InPar,'Tran',@tran2dCl); % in pixels
addOptional(InPar,'TranPar','cheby1_3'); % in pixels

addOptional(InPar,'ColX',1);  % index, name or cell array of names from which to select first
addOptional(InPar,'ColY',2);
addOptional(InPar,'ColMag',3);
addOptional(InPar,'ZP',27);
addOptional(InPar,'MagFluxUnits','mag');  % flux | mag

addOptional(InPar,'RefCat','GAIADR2'); % or a catalog
addOptional(InPar,'Con',{{'ExcessNoise',[0 3]},{'Mag_G',[10 20]}});
addOptional(InPar,'ColRefRA',{'RA'});    % or number, or cell array - first to appear
addOptional(InPar,'ColRefDec',{'Dec'});
addOptional(InPar,'ColRefMag',{'Mag'});
addOptional(InPar,'ColRefColor',{'Color'});
addOptional(InPar,'ColRefAM',{'AM'});
addOptional(InPar,'ColRefPA',{'PA'});
addOptional(InPar,'ObsGeoPos',[]);

addOptional(InPar,'RefMagRange',[13 20]);

addOptional(InPar,'ColRowPar',{});
addOptional(InPar,'OverdensePar',{});


addOptional(InPar,'CatFun','catsHTM');
addOptional(InPar,'OmitNoPM',false);
addOptional(InPar,'Shape','box');       % 'circ' | 'box'
addOptional(InPar,'SizeUnits','deg');
addOptional(InPar,'EpochOut',2451545);
addOptional(InPar,'EpochUnits','JD');   % 'J' | 'B' | 'JD' | 'MJD'
addOptional(InPar,'RefCooUnits','deg');

addOptional(InPar,'CatRemoveNaN',true);
addOptional(InPar,'CatRemoveBadColRow',true);
addOptional(InPar,'CatRemoveOverDense',true);
addOptional(InPar,'EqualizeDensity',true);
addOptional(InPar,'DiluteThreshold',0.5);

% imUtil.patternMatch.match_scale_rot_shift parameters
addOptional(InPar,'ScaleRange',[1.0]); % scale or [min max] range that require to ?
addOptional(InPar,'HistDistEdgesRotScale',[10 600 300]);
addOptional(InPar,'HistDistEdgesRot',(12:3:300).');
addOptional(InPar,'HistRotEdges',(-90:0.2:90));  % rotation or [min max] rotation that require to ?
addOptional(InPar,'RangeX',[-1000 1000]); 
addOptional(InPar,'RangeY',[-1000 1000]); 
addOptional(InPar,'StepX',4); 
addOptional(InPar,'StepY',4); 
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


parse(InPar,varargin{:});
InPar = InPar.Results;


if nargin==0
    % simulation mode
    PWD = pwd;
    cd /home/yossi/matlab/images
    IC = imCl.fits2imCl('PTF*.fits');
    IC = IC(1);
    
    S=FITS.read2sim('PTF*.fits');
    S = S(1);
    W=ClassWCS.populate(S);
    [RA,Dec]=W.xy2coo([1000,2000]);
    
    cd(PWD)
    %S = S.trim_image([500 1500 1500 2500]);
    %S = mextractor(S);
    
    
    %IC.Im = flipud(IC.Im);
    InPar.Flip = [1 -1];
    
    IC = IC.trim([500 1500 1500 2500]);
    IC = gain_correct(IC);
    
    [Cat,ColCell,ResFindSrc]=imUtil.sources.find_measure_sources(IC.Im,'Threshold',5);

    
    %Catalog = S.Cat(:,[2 3 20]);
    Catalog = Cat.Cat(:,[12 13 8]);
    InPar.MagFluxUnits = 'flux';
    
%     Tmp = IC.Header.getVal('CRVAL1');
%     InPar.RA  = Tmp{1}{1};
%     Tmp = IC.Header.getVal('CRVAL2');
%     InPar.Dec  = Tmp{1}{1};
%     Tmp = IC.Header.getVal('PIXSCALE');
%     InPar.Scale  = Tmp{1}{1};

    InPar.RA  = RA.*RAD;
    InPar.Dec = Dec.*RAD +100./3600;
    InPar.Scale = 1.01;
    InPar.CooUnits = 'rad';
    
    InPar.EpochOut = julday(IC.Header);
    Tmp1 = IC.Header.getVal('OBSLON');
    Tmp2 = IC.Header.getVal('OBSLAT');
    
    InPar.ObsGeoPos = [Tmp1{1}{1}, Tmp2{1}{1}];
    InPar.ZP    = 27;

    clear Cat;
end

%--- start here


% Coo units - convert to radians
%InPar.RA  = convert.angular(InPar.CooUnits,'rad',InPar.RA);
%InPar.Dec = convert.angular(InPar.CooUnits,'rad',InPar.Dec);



% get magnitude data from catalog
% if isempty(InPar.ColMag)
%     Mag = [];
% else
%     switch lower(InPar.MagFluxUnits)
%         case 'flux'
%             Mag = convert.flux2mag(Catalog(:,InPar.ColMag),InPar.ZP,true);
%         case 'mag'
%             Mag = Catalog(:,InPar.ColMag);
%         otherwise
%             error('Unknown MagFluxUnits option');
%     end
% end

% if catCl.iscatCl(Catalog)
%     % convert catCl into a matrix
%     InPar.ColX   = col_bestFromList(Catalog,InPar.ColX);
%     InPar.ColY   = col_bestFromList(Catalog,InPar.ColY);
%     if isempty(InPar.ColMag)
%         Cat = Catalog.Cat(:,[InPar.ColX, InPar.ColY]);
%         Cat = [Cat, nan(size(Cat,1),1)];
%     else
%         InPar.ColMag = col_bestFromList(Catalog,InPar.ColMag);
%         Cat = Catalog.Cat(:,[InPar.ColX, InPar.ColY, InPar.ColMag]);
%     end
% else
%     % New version of Cat with [X,Y,Mag]
%     if isempty(InPar.ColMag)
%         Cat = Catalog(:,[InPar.ColX, InPar.ColY]);
%         Cat = [Cat, nan(size(Cat,1),1)];
%     else
%         Cat = Catalog(:,[InPar.ColX, InPar.ColY, InPar.ColMag]);
%     end
% end
% 
% switch lower(InPar.MagFluxUnits)
%     case 'flux'
%         Mag = convert.flux2mag(Cat(:,3),InPar.ZP,true);
%     case 'mag'
%         Mag = Cat(:,3);
%     otherwise
%         error('Unknown MagFluxUnits option');
% end


% generate the transformation object
if tran2dCl.istran2dCl(InPar.Tran)
    Tran = InPar.Tran;
else
    if isa(InPar.Tran,'function_handle')
        Tran = InPar.Tran(InPar.TranPar);
    else
        error('Illegal Tran input');
    end
end


if catCl.iscatCl(Catalog)
    % Catalog is a catCl object
    InPar.ColX   = col_bestFromList(Catalog,InPar.ColX);
    InPar.ColY   = col_bestFromList(Catalog,InPar.ColY);
    if ~isempty(InPar.ColMag)
        InPar.ColMag = col_bestFromList(Catalog,InPar.ColMag);
    end
    
    Cat = Catalog.Cat;
    
else
    % Catalog is a matrix
    Cat = Catalog;
    
end


% get magnitude data from catalog
if isempty(InPar.ColMag)
    Mag = [];
else
    switch lower(InPar.MagFluxUnits)
        case 'flux'
            Mag = convert.flux2mag(Cat(:,InPar.ColMag),InPar.ZP,true);
        case 'mag'
            Mag = Cat(:,InPar.ColMag);
        otherwise
            error('Unknown MagFluxUnits option');
    end
end
% New version of Cat with [X,Y,Mag]
Cat = [Cat(:,[InPar.ColX, InPar.ColY]), Mag];






% get astrometric catalog
[Ref,RefColCell,RefColUnits]=imUtil.cat.astrometric_cat(InPar.RA,InPar.Dec,'Cat',InPar.RefCat,...
                                              'Size',InPar.Size,...
                                              'OmitNoPM',InPar.OmitNoPM,...
                                              'Con',InPar.Con,...
                                              'Shape',InPar.Shape,...
                                              'SizeUnits',InPar.SizeUnits,...
                                              'EpochOut',InPar.EpochOut,...
                                              'CooUnits',InPar.RefCooUnits,...
                                              'ObsGeoPos',InPar.ObsGeoPos,...
                                              'OutClass','mat');
                     
% get column indices
[~,~,ColIndRefRA]    = catCl.fisrt_col_2appear(RefColCell,InPar.ColRefRA);
[~,~,ColIndRefDec]   = catCl.fisrt_col_2appear(RefColCell,InPar.ColRefDec);
[~,~,ColIndRefMag]   = catCl.fisrt_col_2appear(RefColCell,InPar.ColRefMag);
[~,~,ColIndRefColor] = catCl.fisrt_col_2appear(RefColCell,InPar.ColRefColor);
[~,~,ColIndRefAM]    = catCl.fisrt_col_2appear(RefColCell,InPar.ColRefAM);
[~,~,ColIndRefPA]    = catCl.fisrt_col_2appear(RefColCell,InPar.ColRefPA);


% Applay magnitude range
Flag = Ref(:,ColIndRefMag)>InPar.RefMagRange(1) & Ref(:,ColIndRefMag)<InPar.RefMagRange(2);
Ref  = Ref(Flag,:);


% Project catalog from sky to plan
% input is radians
% output is ~pixels
[X,Y] = imUtil.proj.gnomonic(Ref(:,ColIndRefRA),Ref(:,ColIndRefDec),[InPar.RA, InPar.Dec]./RAD,RAD.*ARCSEC_DEG./InPar.Scale,'rad');

% check - ok
%[X,Y] = imUtil.proj.gnomonic((InPar.RA+0.1)./RAD,(InPar.Dec-0.2)./RAD,[InPar.RA, InPar.Dec]./RAD,RAD,'rad')
%[X11,Y11]=celestial.proj.pr_gnomonic((InPar.RA+0.1)./RAD,(InPar.Dec-0.2)./RAD,RAD.*3600./1.01,[InPar.RA, InPar.Dec]./RAD)

% convert to AstCat to pixel scale

% x,y - matched to Ref
RefXY = [X, Y, Ref(:,ColIndRefMag)];


% matching start here


% clean catalog
% remove NaNs
% bad columns
% overdense regions
% equalize surface density
[~,~,FlagCat,FlagRef]=imUtil.patternMatch.prep_cat_for_astrometry(Cat,RefXY,...
                                          'CatRemoveNaN',InPar.CatRemoveNaN,...
                                          'CatRemoveBadColRow',InPar.CatRemoveBadColRow,...
                                          'CatRemoveOverDense',InPar.CatRemoveOverDense,...
                                          'RefRemoveNaN',false,...
                                          'RefRemoveBadColRow',false,...
                                          'RefRemoveOverDense',false,...
                                          'EqualizeDensity',InPar.EqualizeDensity,...
                                          'DiluteThreshold',InPar.DiluteThreshold,...
                                          'ColRowPar',InPar.ColRowPar,...
                                          'OverdensePar',InPar.OverdensePar,...
                                          'CatHalfSize',[],...
                                          'RefHalfSize',[],...
                                          'ColCatX',1,...
                                          'ColCatY',2,...
                                          'ColCatMag',3,...
                                          'ColRefX',1,...
                                          'ColRefY',2,...
                                          'ColRefMag',3);
                                      
% selected sources
Cat   = Cat(FlagCat,:);
Ref   = Ref(FlagRef,:);
RefXY = RefXY(FlagRef,:);
X     = X(FlagRef);
Y     = Y(FlagRef);

                                      
% Shift source catalog relative to image center
if isempty(InPar.SizeCatXY)
    CenterCatX = (max(Cat(:,1)) + min(Cat(:,1))).*0.5; %median(Cat(:,1));
    CenterCatY = (max(Cat(:,2)) + min(Cat(:,2))).*0.5; %median(Cat(:,2));
else
    CenterCatX = InPar.SizeCatXY(1).*0.5;
    CenterCatY = InPar.SizeCatXY(2).*0.5;
end
    
CatXY = Cat(:,1:2) - [CenterCatX, CenterCatY];
%CatXY = Cat(:,1:2) - [500, 500];

% 
% Cat0 = Catalog(:,12:13) - [500 500];
% 
% Cat0 = Catalog(:,2:3) - [1000 2000];
% 
% 

%[Sol,ResS,Res]=imUtil.patternMatch.match_scale_rot_shift(CatXY,RefXY,'Flip',InPar.Flip,'HistRotEdges',(-90:0.1:90));
[Sol,ResS,ResShiftMatched]=imUtil.patternMatch.match_scale_rot_shift(CatXY,RefXY,'Scale',InPar.ScaleRange,...
                                                                                 'HistDistEdgesRotScale',InPar.HistDistEdgesRotScale,...
                                                                                 'HistDistEdgesRot',InPar.HistDistEdgesRot,...
                                                                                 'HistRotEdges',InPar.HistRotEdges,...
                                                                                 'RangeX',InPar.RangeX,...
                                                                                 'RangeY',InPar.RangeY,...
                                                                                 'StepX',InPar.StepX,...
                                                                                 'StepY',InPar.StepY,...
                                                                                 'Flip',InPar.Flip,...
                                                                                 'SearchRadius',InPar.SearchRadius,...
                                                                                 'MaxMethod',InPar.MaxMethod,...
                                                                                 'Threshold',InPar.Threshold,...
                                                                                 'FracOfMax',InPar.FracOfMax,...
                                                                                 'Conn',InPar.Conn,...
                                                                                 'BackFun',InPar.BackFun,...
                                                                                 'BackFunPar',InPar.BackFunPar',...
                                                                                 'VarFun',InPar.VarFun,...
                                                                                 'VarFunPar',InPar.VarFunPar,...
                                                                                 'SubSizeXY',InPar.SubSizeXY,...
                                                                                 'Overlap',InPar.Overlap,...
                                                                                 'MinVariance',InPar.MinVariance,...
                                                                                 'CatColX',1,...
                                                                                 'CatColY',2,...
                                                                                 'RefColX',1,...
                                                                                 'RefColY',2);
                                                                                 
                                                                             




%... so far so good
if isempty(Sol.SN)
    Res = [];
else
    % applay transformation to Ref
    %[Theta, Scale, ShiftX, ShiftY, FlipX, FlipY]
    I = 1;
    %AffinePar = [Sol.Rot(I), Sol.Scale(I), Sol.ShiftX(I), Sol.ShiftY(I),  Sol.Flip(I,:)];
    AffinePar = Sol.AffineTran{I};
    [RefNewX,RefNewY]=imUtil.cat.affine2d_transformation(RefXY,AffinePar,'+');
    RefNewXY = [RefNewX,RefNewY];

    % match stars
    [ResMatched,MatchedCat,MatchedRef]=imUtil.cat.match_sources_xy(CatXY,RefNewXY,'IsCatSorted',false,...
                                                                        'SearchRadius',5);

    %--- fit transformation between MatchedCat and RefXY ---
    % add : 'Mag','Color','AM','PA', ModXpix, ModYpix to Ref catalog
    % X,Y are the projected refernce catalog coordinates
    
    RefM = [X, Y, Ref(:,[ColIndRefMag, ColIndRefColor, ColIndRefAM, ColIndRefPA])];


    % input units are pixels
    [Param,Resid,ResLoop,Tran]=imUtil.patternMatch.fit_astrometric_tran(MatchedCat,RefM,'Tran',Tran);

    TranCenter.RA       = InPar.RA;
    TranCenter.Dec      = InPar.Dec;
    TranCenter.CooUnits = InPar.CooUnits;
    TranCenter.Scale    = InPar.Scale;
    TranCenter.CRPIX    = [CenterCatX, CenterCatY];
    TranCenter.EQUINOX  = 2000;

    Res.TranCenter = TranCenter;
    Res.Tran  = Tran;
    Res.Sol   = Sol;
    Res.Param = Param;
    Res.Resid = Resid;
    Res.Loop  = ResLoop;
    Res.Cat.MatchedCat     = MatchedCat;
    % MatchedCat with the origibal coordinate system
    Res.Cat.MatchedCatOrig = MatchedCat + [CenterCatX, CenterCatY];
    Res.Cat.CenterCat      = [CenterCatX, CenterCatY];
    Res.Cat.MatchedRefXY   = RefM;
    Res.Cat.MatchedRef     = Ref;
end






