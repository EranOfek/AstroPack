function [Tran, ParWCS, ResFit] = fitWCS(Xcat, Ycat, Xref, Yref, Mag, RAdeg, Decdeg, Args)
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
    %            'ProjType' - Projection type. See imProc.trans.projection.
    %                   Default is 'TPV'.
    %            'TranMethod' - ['TPV'] | 'tran2d'
    %                   This dictates the fitting scheme.
    %            'Tran' - A Tran2D object for the transformation to fit.
    %                   Default is Tran2D.
    %            'UseFlag' - - A vector of logicals indicating which
    %                   sources (in the vectors of coordinates) to
    %                   use. Default is true.
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
    %                   Default is @nanmedian.
    %            'FunStd' - A function handle to use when calculating the std
    %                   of the data in each bin, or when calculating the global
    %                   std after the polynomial fit.
    %                   Default is @imUttil.background.rstd.
    %            'InterpMethod' - Interpolation method. Default is 'linear'.
    %            'ThresholdSigma' - Threshold in sigmas (std) for flagging good
    %                   data. Default is 3.
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
        Args.ProjType           = 'TPV';
        Args.TranMethod         = 'TPV';
        Args.Tran               = Tran2d;
        Args.UseFlag logical    = true;
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
        
    end
    ARCSEC_DEG = 3600;


    switch lower(Args.TranMethod)
        case 'tpv'
            % The following fitting attempt to mimic the order
            % of the TAN-TPV transformation


            % fit affine transformation (only)
            % Units are "pixels"
            Tran1 = Tran2D('poly1');
            % note that independent and dependent coordinates change
            % order
            [Tran1, ResFit] = fitAstrometricTran(Tran1,...
                                    Xref, Yref,...
                                    Xcat, Ycat,...
                                    'UseFlag',Args.UseFlag,...
                                    'ExtraData',[],...
                                    'Mag',Mag,...
                                    'ErrPos',Args.ErrPos,...
                                    'Niter',1,...
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
            %
            CRPIX1     = Tran1.ParX(1);
            CRPIX2     = Tran1.ParY(1);
            % CD matrix
            CD         = [Tran1.ParX(2:3).'; Tran1.ParY(2:3).'];  % [pix]
            % measured scale in arcsec/pix
            ScaleASpix = Args.Scale;   % "/pix
            CD         = CD.*ScaleASpix./ARCSEC_DEG;   % [deg]

            % Apply affine transformation on Xcat, Ycat
            Xsi  = CD(1,1).*(Xcat - CRPIX1) + CD(1,2).*(Ycat - CRPIX2);   % [deg]
            Eta  = CD(2,1).*(Xcat - CRPIX1) + CD(2,2).*(Ycat - CRPIX2);   % [deg]

            % convert Xref, Yref to deg
            XrefT = Xref.*ScaleASpix./ARCSEC_DEG;   % [deg]
            YrefT = Yref.*ScaleASpix./ARCSEC_DEG;   % [deg]

            % parameters from which to construct WCS
            ParWCS.CRPIX  = [CRPIX1, CRPIX2] + Args.ImageCenterXY(:).';
            ParWCS.CRVAL  = [RAdeg, Decdeg];
            ParWCS.CD     = CD;
            ParWCS.CUNIT  = {'deg', 'deg'};
            ParWCS.CTYPE  = {sprintf('RA---%s',upper(Args.ProjType)), sprintf('DEC--%s',upper(Args.ProjType))};
            ParWCS.NAXIS  = 2;

            [Tran, ResFit] = fitAstrometricTran(Args.Tran,...
                                    XrefT, YrefT,...
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
                                'a'

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


    
    
end
    