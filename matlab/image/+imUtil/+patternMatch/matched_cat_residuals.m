function Res=matched_cat_residuals(Cat,Ref,varargin)
% Calculate the astrometric residuals and errors between two matched catalogs
% Package: imUtil.patternMatch
% Description: Given a catalog (Cat) and a reference catalog (Ref), both
%              containing astrometric X/Y measurments of matched sources
%              (i.e., each line in Cat corresponds to the same line in
%              Ref), calculate the astrometric residuals. 
%              If a mgnitude vector (per source) is given, then calculate
%              an histogram (or fit a polynomial) of the residuals as a
%              function of magnitude. The median residal at each magnitude
%              is returned as the assumed astrometric error for each
%              source.
% Input  : - A catalog (at least two column matrix).
%          - A reference catalog (at least two column matrix).
%          * Pairs of ...,key,val,... Possible keywords include:
%            'Mag' - An optional column containing the magnitude for each
%                   source.
%                   If empty, then calculate only the std of the residauls.
%            'MagMethod' - Method by which to fit the residuals.
%                   Options are:
%                   'poly' - Fit a polynomial to residuals vs. mag in two
%                       iteartions.
%                   'hist' - calculate an histogram.
%                   Default is 'hist'.
%            'HistBin' - Bin size for hsitogram. Default is 0.5 mag.
%            'HistRange' - Gistogram range [low high]. If [NaN NaN] use
%                       min, max of magnitudes. Default is [NaN NaN].
%            'PolyOrder - Polynomial order to fit. Default is 3.
%            'Nsigma'   - Number of sigma for sigma clipping in polynomial
%                       fitting. Default is 3.
%            'InterpMethod' - Interpolation method when calcualting the
%                       error as a function of mag.
%                       Default is 'linear'.
%            'CatColX' - Catalog column that contains the X axis. Default is 1.
%            'CatColY' - Catalog column that contains the Y axis. Default is 2.
%            'RefColX' - Reference column that contains the X axis. Default is 1.
%            'RefColY' - Reference column that contains the Y axis. Default is 2.
% Output : - A structure containing the following fields:
%            'Err' - Vector of errors per source, as interpolated from the
%                   residuals vs. magnitude.
%            'MinErr' - Min of Err.
%            'ResidStD' - StD of Resid vector.
%            'Resid' - Vector of residuals per source.
%            'DX'    - Residuals in X-axis per source.
%            'DY'    - Residuals in Y-axis per source.
%            'Chi2'  - \chi^2
%            'Nobs'  - Number of observations.
%            'Number of degrees of freedom.
% License: GNU general public license version 3
% Tested : Matlab R2015b
%     By : Eran O. Ofek                    Apr 2016
%    URL : http://weizmann.ac.il/home/eofek/matlab/
% Example: Res=imUtil.patternMatch.matched_cat_residuals(rand(100,2),rand(100,2),'Mag',rand(100,1).*10)
% Reliable: 2


InPar = inputParser;

addOptional(InPar,'Mag',[]);
addOptional(InPar,'MagMethod','hist');
addOptional(InPar,'HistBin',0.5);
addOptional(InPar,'HistRange',[NaN NaN]);

addOptional(InPar,'PolyOrder',3);
addOptional(InPar,'Nsigma',3);

addOptional(InPar,'InterpMethod','linear');

addOptional(InPar,'CatColX',1);
addOptional(InPar,'CatColY',2);
addOptional(InPar,'RefColX',1);
addOptional(InPar,'RefColY',2);


parse(InPar,varargin{:});
InPar = InPar.Results;



DX = Cat(:,InPar.CatColX) - Ref(:,InPar.RefColX);
DY = Cat(:,InPar.CatColY) - Ref(:,InPar.RefColY);
Resid = sqrt(DX.^2 + DY.^2);
Nobs  = numel(Resid);

% calculate Resid vs. Mag
if ~isempty(InPar.Mag)
    switch lower(InPar.MagMethod)
        case 'hist'
            B = timeSeries.bin.binning([InPar.Mag, Resid],InPar.HistBin,InPar.HistRange,{'MidBin',@nanmedian,@numel});
            Err = interp1(B(:,1),B(:,2),InPar.Mag,InPar.InterpMethod);
            Ndof = Nobs - sum(B(:,3)>1);
            
        case 'poly'
            % normalize Mag
            NormMag = (InPar.Mag - mean(InPar.Mag))./range(InPar.Mag);
            Par = polyfit(NormMag,Resid,InPar.PolyOrder);
            Y   = polyvar(Par,NormMag);
            StdRes = std(Resid - Y);
            % select poinys (sigma clipping) for second iteration
            FlagGood = Resid<(Y + StdRes.*InPar.Nsigma);
            Par      = polyfit(NormMag(FlagGood),Resid(FlagGood),InPar.PolyOrder);
            Err      = polyvar(Par,NormMag);
            Ndof     = Nobs - InPar.PolyOrder;
            
        otherwise
            error('(Unknown MagMethod option');
    end
    Chi2     = nansum((Resid./Err).^2);
else
    Err  = [];
    Chi2 = [];
    Ndof = [];
end

Res.Err      = Err;
Res.MinErr   = min(Err);
Res.ResidStD = std(Resid);
Res.Resid    = Resid;
Res.DX       = DX;
Res.DY       = DY;
Res.Chi2     = Chi2;
Res.Nobs     = Nobs;
Res.Ndof     = Ndof;

