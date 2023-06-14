function [Tran, ParWCS, ResFit, WCS] = fitWCS(Xcat, Ycat, Xref, Yref, Mag, RAdeg, Decdeg, Args)
    % Perform the Tran2D.fitAstrometricTran and prepare the WCS info
    %   This is an auxilary function that performs the fitting stage
    %   between an astrometric catalog and an image catalog, and return the
    %   Tran2D object as well as the information needed for the WCS (e.g.,
    %   CRPIX, CRVAL, etc.).
    % Input  : - Xcat - Vector of catalog X coordinates
    %          - Ycat - Vector of catalog Y coordinates
    %          - Xref - Vector of external reference projected X
    %               coordinates
    %          - Yref - Vector of external reference projected Y
    %               coordinates
    %          - Mag - Vector of magnitudes
    %          - RAdeg - RA [deg] of the center for projection.
    %          - Decdeg - Dec [deg] of the center for projection.
    %          * ...,key,val,...
    %            'ImageCenterXY'
    %            'Scale' - Scale ["/pix]
    %            'Flip'  - [X Y] flip operated already on the coordinates.
    %                   Default is [1 1].
    %            'ProjType' - Projection type. See imProc.trans.projection.
    %                   Default is 'TPV'.
    %            'TranMethod' - ['TPV'] | 'tran2d'
    %                   This dictates the fitting scheme.
    %            'Tran' - A Tran2D object for the transformation to fit.
    %                   Default is Tran2D.
    %            'ExtraData' - Additional columns to pass to the Tran2D
    %                   transformation. Default is [].
    %            'ErrPos' - Error in positions [pix].
    %            'Niter' - Number of fitting iterations.
    %            'FitMethod' - Fitting method for Tran2D/fitAstrometricTran
    %                   Default is 'lscov'.
    %            'MaxResid' - Maximum residual to use in fit.
    %                   Default is 1.
    %            'MagRange' - [Min Max] max range. Default is [].
    %            'BinMethod' - Method to use:
    %                   'poly' - polynomial fit.
    %                   'bin' - binning the data.
    %                   Default is 'bin'
    %            'PolyDeg' - Polynomial degree for the polynomial fit.
    %                   Default is 3.
    %            'BinSize' - Bin size for binning. Default is 1 (mag).
    %            'FunMean' - A function handle to use when calculating the mean
    %                   of the data in each bin.
    %            'FunStd' - A function handle to use when calculating the std
    %                   of the data in each bin, or when calculating the global
    %                   std after the polynomial fit.
    %                   Default is @imUttil.background.rstd.
    %            'InterpMethod' - Interpolation method. Default is 'linear'.
    %            'ThresholdSigma' - Threshold in sigmas (std) for flagging good
    %                   data. Default is 3.
    %            'TestNbin' - Number of binx in each dimensions, when
    %                   counting the number of good sources as a function of
    %                   position in the image. Default is 3.
    % Output : - The Tran2D object with the fitted transformations
    %          - (ParWCS) A structure with some of the parameters required
    %            in order to build the WCS.
    %          - (ResFit) A structure containing information about the fit
    %            quality. See Tran2D/fitAstrometricTran for details.
    % Author : Eran Ofek (Jul 2021)
    % Example: imProc.astrometry.fitWCS
    
    arguments
        Xcat
        Ycat
        Xref
        Yref
        Mag
        RAdeg
        Decdeg
        Args.ImageCenterXY
        Args.Scale              
        Args.Flip               = [1 1];
        Args.ProjType           = 'TPV';
        Args.TranMethod         = 'TPV';
        Args.Tran               = Tran2D;
        Args.ExtraData          = [];
        Args.ErrPos
        Args.Niter
        Args.FitMethod          = 'lscov';
        Args.MaxResid
        Args.MagRange
        Args.BinMethod
        Args.PolyDeg
        Args.BinSize
        Args.FunMean
        Args.FunStd
        Args.InterpMethod
        Args.ThresholdSigma
        Args.TestNbin             = 3;
        Args.RegionalMaxMedianRMS = 1;     % arcsec OR pix?
        Args.RegionalMaxWithNoSrc = 0;
        Args.MaxErrorOnMean       = 0.05;  % arcsec OR pix?
    end
    ARCSEC_DEG = 3600;

    switch lower(Args.TranMethod)
        case 'tan'
            % simple-TAN with no iterations!!
            
            
            ScaleASpix = Args.Scale;   % "/pix
            % convert Xref, Yref to deg
            ScalingDegInPix = ScaleASpix./ARCSEC_DEG;
            
            % new code
            CRVAL1 = RAdeg;
            CRVAL2 = Decdeg;
            
            Xpix   = Xcat + Args.ImageCenterXY(1);
            Ypix   = Ycat + Args.ImageCenterXY(2);
            
            
            RefRA  = Xref.*ScalingDegInPix;   % [deg] (RA in ref -CRVAL1)
            RefDec = Yref.*ScalingDegInPix;   % [deg] (Dec in ref -CRVAL2)
            Nsrc   = numel(RefRA);
            
            Hx = [ones(Nsrc,1), Xpix, Ypix];
            Hy = [ones(Nsrc,1), Xpix, Ypix];
            Flag = ~isnan(RefRA) & ~isnan(Xpix);
            
            ParX = Hx(Flag,:)\RefRA(Flag);
            ParY = Hy(Flag,:)\RefDec(Flag);
            
            CD   = [ParX(2), ParX(3); ParY(2), ParY(3)];
            
            A = ParX(1);
            B = ParY(1);
            Det = CD(2,2).*CD(1,1) - CD(2,1).*CD(1,2);
            CRPIX1 = (-A.*CD(2,2) + B.*CD(1,2))./Det;
            CRPIX2 = (A.*CD(2,1) - B.*CD(1,1))./Det;
            
            
            % parameters from which to construct WCS
            ParWCS.CRPIX  = [CRPIX1, CRPIX2];
            ParWCS.CRVAL  = [RAdeg, Decdeg];
            ParWCS.CD     = CD;
            ParWCS.CUNIT  = {'deg', 'deg'};
            ParWCS.CTYPE  = {sprintf('RA---%s',upper(Args.ProjType)), sprintf('DEC--%s',upper(Args.ProjType))};
            ParWCS.NAXIS  = 2;

            Tran = Tran2D('poly1');
            Tran.ParX = zeros(1,3);
            Tran.ParY = zeros(1,3);
            
            
            ResFit.ResidX = RefRA  - Hx*ParX;
            ResFit.ResidY = RefDec - Hy*ParY;
            ResFit.Resid  = sqrt(ResFit.ResidX.^2 + ResFit.ResidY.^2);
            ResFit.FlagSrc      = Flag;
            
            ResFit.Ngood        = sum(Flag);
            ResFit.AssymRMS_mag = NaN;
            ResFit.ErrorOnMean  = NaN;
            
            
        case 'tpv'
            % The following fitting attempt to mimic the order
            % of the TAN-TPV transformation

            ScaleASpix = Args.Scale;   % "/pix
            % convert Xref, Yref to deg
            ScalingDegInPix = ScaleASpix./ARCSEC_DEG;
            
            % new code
            CRVAL1 = RAdeg;
            CRVAL2 = Decdeg;
            
            Xpix   = Xcat + Args.ImageCenterXY(1);
            Ypix   = Ycat + Args.ImageCenterXY(2);
            
            
            RefRA  = Xref.*ScalingDegInPix;   % [deg] (RA in ref -CRVAL1)
            RefDec = Yref.*ScalingDegInPix;   % [deg] (Dec in ref -CRVAL2)
            Nsrc   = numel(RefRA);
            
            Hx = [ones(Nsrc,1), Xpix, Ypix];   % < is this correct?? no minus?
            Hy = [ones(Nsrc,1), Xpix, Ypix];
            Flag = ~isnan(RefRA) & ~isnan(Xpix);
            
            ParX = Hx(Flag,:)\RefRA(Flag);
            ParY = Hy(Flag,:)\RefDec(Flag);
            
            CD   = [ParX(2), ParX(3); ParY(2), ParY(3)];
            % apply flip to CD
            %CD   = CD.*[Args.Flip(1), -Args.Flip(2); Args.Flip(1), Args.Flip(2)];
            
            A = ParX(1);
            B = ParY(1);
            Det = CD(2,2).*CD(1,1) - CD(2,1).*CD(1,2);
            CRPIX1 = (-A.*CD(2,2) + B.*CD(1,2))./Det;
            CRPIX2 = (A.*CD(2,1) - B.*CD(1,1))./Det;
            
            
            % parameters from which to construct WCS
            ParWCS.CRPIX  = [CRPIX1, CRPIX2];
            ParWCS.CRVAL  = [CRVAL1, CRVAL2];
            ParWCS.CD     = CD;
            ParWCS.CUNIT  = {'deg', 'deg'};
            ParWCS.CTYPE  = {sprintf('RA---%s',upper(Args.ProjType)), sprintf('DEC--%s',upper(Args.ProjType))};
            ParWCS.NAXIS  = 2;

%             Tran = Tran2D('poly1');
%             Tran.ParX = zeros(1,3);
%             Tran.ParY = zeros(1,3);    
%             ResFit.ResidX = RefRA  - Hx*ParX;
%             ResFit.ResidY = RefDec - Hy*ParY;
%             ResFit.Resid  = sqrt(ResFit.ResidX.^2 + ResFit.ResidY.^2);
%             ResFit.Ngood = sum(Flag);
%             ResFit.AssymRMS_mag = NaN;
%             ResFit.ErrorOnMean = NaN;
            
            
            % Apply affine transformation on Xcat, Ycat
            Xsi  = CD(1,1).*(Xpix - CRPIX1) + CD(1,2).*(Ypix - CRPIX2);   % [deg]
            Eta  = CD(2,1).*(Xpix - CRPIX1) + CD(2,2).*(Ypix - CRPIX2);   % [deg]

            
            [Tran, ResFit] = fitAstrometricTran(Args.Tran,...
                                    RefRA, RefDec,...
                                    Xsi, Eta,...
                                    'ExtraData',[],...
                                    'Mag',Mag,...
                                    'ErrPos',Args.ErrPos,...
                                    'Niter',Args.Niter,...
                                    'FitMethod',Args.FitMethod,...
                                    'MaxResid',Args.MaxResid,...
                                    'MagRange',Args.MagRange,...
                                    'BinMethod',Args.BinMethod,...
                                    'PolyDeg',Args.PolyDeg,...
                                    'BinSize',Args.BinSize,...
                                    'FunMean',Args.FunMean,...
                                    'FunStd',Args.FunStd,...
                                    'InterpMethod',Args.InterpMethod,...
                                    'ThresholdSigma',Args.ThresholdSigma);
            % make sure that the PV doesn't introduce flips
%             SignX = sign(Tran.ParX(2:3));
%             SignY = sign(Tran.ParY(2:3));
%             %ParWCS.CD(1,:) = ParWCS.CD(1,:).*SignX(:).';
%             %ParWCS.CD(2,:) = ParWCS.CD(2,:).*SignY(:).';
%             
%             ParWCS.CD(1,:) = ParWCS.CD(:,1).*SignX(:);
%             ParWCS.CD(2,:) = ParWCS.CD(:,2).*SignY(:);
%             Tran.ParX(2:3) = abs(Tran.ParX(2:3));
%             Tran.ParY(2:3) = abs(Tran.ParY(2:3));
            
        case 'tran2d'
            % a full Tran2D solution, where the observations
            % are the independent variable
            % and the fit is done in pixel (observations) units
            % i.e., the logical thing to do
            [Tran, ResFit] = fitAstrometricTran(Args.Tran,...
                                    Xcat, Ycat,...
                                    Xref,Yref,...
                                    'ExtraData',[],...
                                    'Mag',Mag,...
                                    'ErrPos',Args.ErrPos,...
                                    'Niter',Args.Niter,...
                                    'FitMethod',Args.FitMethod,...
                                    'MaxResid',Args.MaxResid,...
                                    'MagRange',Args.MagRange,...
                                    'BinMethod',Args.BinMethod,...
                                    'PolyDeg',Args.PolyDeg,...
                                    'BinSize',Args.BinSize,...
                                    'FunMean',Args.FunMean,...
                                    'FunStd',Args.FunStd,...
                                    'InterpMethod',Args.InterpMethod,...
                                    'ThresholdSigma',Args.ThresholdSigma);


%                 [Param, Res, Tran] = imProc.trans.fitTransformation(FilteredProjAstCat, MatchedCat,...
%                                                               'Tran',Args.Tran,...
%                                                               'Norm',NaN,...
%                                                               'ColRefX',Args.CatColNamesX,...
%                                                               'ColRefY',Args.CatColNamesY,...
%                                                               'ColCatX',RefColNameX,...
%                                                               'ColCatY',RefColNameY);
        otherwise
            error('Unknown TranMethod option');
    end

    ResFit.SrcX  = Xcat + Args.ImageCenterXY(1);
    ResFit.SrcY  = Ycat + Args.ImageCenterXY(2);
    
    if nargout>3
        % Generate an AstroWCS - doesnt work for the tran2d option
        KeyValWCS = namedargs2cell(ParWCS);
        WCS = AstroWCS.tran2wcs(Tran, KeyValWCS{:});
        WCS.ResFit   = ResFit;
        
        % sucess
        WCS = populateSucess(WCS, 'TestNbin',Args.TestNbin,...
                                  'RegionalMaxMedianRMS',Args.RegionalMaxMedianRMS,...
                                  'RegionalMaxWithNoSrc',Args.RegionalMaxWithNoSrc,...
                                  'MaxErrorOnMean',Args.MaxErrorOnMean);
      
        
    end
    
end
    