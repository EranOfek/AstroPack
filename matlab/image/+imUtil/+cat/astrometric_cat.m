function [Cat,ColCell,ColUnits]=astrometric_cat(RA,Dec,varargin)
% Get astrometric catalog, corrected for proper motion
% Package: +imUtil.cat
% Input  : - J2000.0 R.A. [deg].
%          - J2000.0 Dec. [deg].
%          * Pairs of ...,key,val,... arguments.
%            The possible keywords are:
%            'Cat' - Catalog. Default is 'GAIADR2'.
%            'CatFun' - Method by which to extract catalog.
%                   Default is 'catsHTM'.
%            'OmitNoPM' - Omit stars with no PM. Default is false.
%            'Con' - constrainf function to applay to catalog.
%                   Default is {{'ExcessNoise',[0 10]},{'Mag_G',[10 20]}}.
%            'Size' - Extraction size [semi width, semi height]
%                   or search radius. Default is [1 1].
%            'Shape' - Extraction shape: 'circ'|'box'. Default is 'box'.
%            'SizeUnits' - Units for size argument. Default is 'deg'.
%            'EpochOut'  - Default is 2451545.
%            'EpochUnits' - Units of EpochOut. Default is 'JD'.
%            'CooUnits' - Input search coordinates inits.
%                    Defaut is 'deg'.
%            'ObsGeoPos' - Observatory Geodetic position [Lon, Lat] in deg.
%                    This is needed for adding the AirMass and Paralactic
%                    Angle. If not provided, then will not be added.
%                    Defualt is [].
%            'AddGeo' - Add [AirMass, ParallacticAngle, Az, Alt] which
%                    depands on Geodetic position and time.
%            'OutClass' - Output object. Options are:
%                    'mat' - A matrix.
%                    'catCl' - A catCl object.
%                    'AstCat' - An AstCat object.
%                    'table' - A table object.
%                    Default is 'mat'.
% Output : - An astrometric catalog with [RA, Dec, Mag, Color, AstErr] columns.
%          - A cell array of colum names.
%          - A cell array of column units.
%      By: Eran O. Ofek                         Apr 2020
% Example: [Cat,ColCell,ColUnits]=imUtil.cat.astrometric_cat(1,1,'EpochOut',2021)
%          [Cat,ColCell,ColUnits]=imUtil.cat.astrometric_cat(1,1,'EpochOut',2021,'ObsGeoPos',[35 32])


RAD = 180./pi;

InPar = inputParser;

addOptional(InPar,'Cat','GAIADR2');
addOptional(InPar,'CatFun','catsHTM');
addOptional(InPar,'OmitNoPM',false);
addOptional(InPar,'Con',{{'ExcessNoise',[0 3]},{'Mag_G',[10 20]}});
addOptional(InPar,'Size',[1 1]);
addOptional(InPar,'Shape','box');       % 'circ' | 'box'
addOptional(InPar,'SizeUnits','deg');
addOptional(InPar,'EpochOut',2451545);
addOptional(InPar,'EpochUnits','JD');   % 'J' | 'B' | 'JD' | 'MJD'
addOptional(InPar,'CooUnits','deg');
addOptional(InPar,'ObsGeoPos',[]);
addOptional(InPar,'OutClass','mat');    % 'mat' | 'AstCat' | 'catCl'

parse(InPar,varargin{:});
InPar = InPar.Results;



% convert RA/Dec to radians
if ischar(RA)
    RA = celestial.coo.convertdms(RA,'gH','r');
else
    RA  = convert.angular(InPar.CooUnits,'rad',RA);
end
if ischar(Dec)
    Dec = celestial.coo.convertdms(Dec,'gD','R');
else
    Dec = convert.angular(InPar.CooUnits,'rad',Dec);
end

% convert Size o arcsec
Size = convert.angular(InPar.SizeUnits,'rad',InPar.Size);   % rad

% treat box/circ
switch lower(InPar.Shape)
    case 'box'
        if numel(Size)==1
            Size = [Size, Size];
        end
        Radius = convert.angular('rad','arcsec',sqrt(sum(Size.^2)));  % arcsec
    case 'circ'
        Radius = convert.angular('rad','arcsec',Size(1));   % arcsec
    otherwise
        error('Unknown Shape option');
end




switch lower(InPar.CatFun)
    case 'catshtm'
        
        switch lower(InPar.Cat)
            case 'gaiadr2'
                [Cat,~] = catsHTM.cone_search('GAIADR2',RA,Dec,Radius,'Con',InPar.Con);

                ColRA      = 1; 
                ColDec     = 2;
                ColEpoch   = 3;
                ColErrRA   = 4;
                ColErrDec  = 5;
                ColPlx     = 6;
                ColPMRA    = 8;
                ColPMDec   = 10;
                ColExNoise = 13;
                ColMag     = 16;
               
                
                RadVel     = 0;
                EpochInRA  = convert.time(Cat(:,ColEpoch),'J','JD');
                EpochInDec = EpochInRA;
                CRA        = Cat(:,ColRA);   % rad
                CDec       = Cat(:,ColDec); 
                PMRA       = Cat(:,ColPMRA); % mas/yr
                PMDec      = Cat(:,ColPMDec); % mas/yr
                Plx        = Cat(:,ColPlx);   % mas
                Mag        = Cat(:,ColMag);
                Color      = Cat(:,18) - Cat(:,20);
                AstErr     = Cat(:,ColExNoise);
                                
                
            otherwise
                error('Unsupported Cat option');
        end
    otherwise
        error('Unsupported Method option');
end



EpochOutJD = convert.time(InPar.EpochOut,InPar.EpochUnits,'JD');

Plx(Plx<1e-4) = 1e-4;

% applay PM/Plx
[CpmRA,CpmDec] = celestial.coo.proper_motion_parallax(EpochOutJD,EpochInRA,EpochInDec,CRA,CDec,PMRA,PMDec,Plx,RadVel);
CatA      = [CpmRA, CpmDec, Mag, Color, AstErr];
ColCell  = {'RA','Dec','Mag','Color','AstErr'};
ColUnits = {'rad','rad','mag','mag','mas'};

if InPar.OmitNoPM
    Flag = isnan(PMRA) | isnan(PMDec) | isnan(Plx);
    CatA = CatA(~Flag,:);
else
    % replace NaNs with original RA,Dec (no PM applied)
    Flag = isnan(PMRA) | isnan(PMDec) | isnan(Plx);
    CatA(Flag,1) = CRA(Flag);
    CatA(Flag,2) = CDec(Flag);
end
    

% add info based on geodetic data
if ~isempty(InPar.ObsGeoPos)
    LST = celestial.time.lst(EpochOutJD,InPar.ObsGeoPos(1)./RAD);
    HorizCoo = celestial.coo.horiz_coo(CatA,EpochOutJD,InPar.ObsGeoPos./RAD,'h');
    Az  = HorizCoo(:,1);
    Alt = HorizCoo(:,2);
    AM = celestial.coo.hardie(pi./2 - Alt);
    PA = celestial.coo.parallactic_angle(CatA(:,1:2), LST, InPar.ObsGeoPos(2)./RAD);
    
    CatA     = [CatA, AM, PA, Az, Alt];
    ColCell  = [ColCell, {'AM','PA','Az','Alt'}];
    ColUnits = [ColUnits, {'','rad','rad','rad'}];
end



% apply precession: TBD


% select in box
switch lower(InPar.Shape)
    case 'box'
        % select source in box region
        Flag = celestial.coo.in_box(CatA(:,1),CatA(:,2),[RA Dec], Size);
        CatA = CatA(Flag,:);
        
end


% convert to output catalog
switch lower(InPar.OutClass)
    case 'mat'
        Cat = CatA;
        
    case 'catcl'
        C = catCl;
        C.Cat      = CatA;
        C.ColCell  = ColCell;
        C.ColUnits = ColUnits;
        Cat        = C;
    case 'astcat'
        C = AstCat;
        C.Cat      = CatA;
        C.ColCell  = ColCell;
        C          = colcell2col(C);
        C.ColUnits = ColUnits;
        Cat        = C;
    case 'table'
        Cat = array2table(CatA);
        Cat.Properties.VariableNames = ColCell;
        Cat.Properties.VariableUnits = ColUnits;
        
    otherwise
        error('Unknown OutClass option');
end
        