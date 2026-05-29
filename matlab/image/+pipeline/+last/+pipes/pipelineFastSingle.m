function [PhotData, Shift] = pipelineFastSingle(Image, CI, XY, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2026 Apr) 
    % Example: 

    arguments
        Image    % current image to analyze
        CI       % CalibImages object
        XY       % positions to analyze
        Args.Gain              = 0.75;  % [e-/adu]
        Args.RA                = []; % RA associated with X/Y
        Args.Dec               = []; % Dec associated with X/Y
        Args.StampHalfSize     = 12;
        Args.AperRadius        = [2 4 6];
        Args.Annulus           = [10 12];
        Args.MomNiter          = 8;
        Args.SigmaWidth        = 1.5;  % sigma-width for weighted 1st moment
        Args.Truncate          = 3;    % 1st mom trunction in units of sigma-width
        Args.MaxStepSize       = [0.1 0.1];
        Args.AperPhotMethod    = 'simple'; % 'simple'|'interp'|'withbs'
    end
    StampSize = Args.StampHalfSize.*2 + 1;

    RoundXY = round(XY);

    % convert to single
    Image = single(Image);

    % apply basic calibration
    % dark subtraction + flat division + gain correct
    Image = Image - CI.Bias.ImageData.Data;
    Image = Image./CI.Flat.ImageData.Data;
    Image = Image.*Gain;

    % cutouts around selected sources
    % Returns a cube of sources
    Cube = imUtil.cut.mex.imageCutouts(Image, RoundXY(:,1), RoundXY(:,2),  StampSize, NaN); 4D   %(0.16s on 3e4 sources)
    Cube = squueze(Cube);  % 3D

    % annulus back/var estimation
    % output: back subtracted cube; vector of back; vector of std
    [CubeBS, Back, Std] = imUtil.sources.mex.annulus_median(Cube, Args.Annulus, 0);

    % 1st moment estimation
    [X1, Y1, ConvergeIter] = imUtil.sources.mex.moment1_cube(CubeBS, SN, Args.MomNiter, Args.SigmaWidth, Args.Truncate, false, Args.MaxStepSize(1), Args.MaxStepSize(2));   %(0.04s on 3e4 sources).
    % X1, Y1 are relative to the stamp corner / use false for relative to
    % center

    % aper photometry
    % not clear relative to what?
    % several options:
    switch Args.AperPhotMethod
        case 'simple'
            [AperPhot, AperArea]=imUtil.sources.mex.aper_phot_cube_simple(Cube, Back, X1, Y1, Args.AperRadius);
        case 'interp'
            [AperPhot, AperArea]=imUtil.sources.mex.aper_phot_cube_interp(Cube, Back, X1, Y1, Args.AperRadius);
        case 'withbs'
            [AperPhot, Back, BackStd, AperArea, AnnArea] = imUtil.sources.mex.aperPhotBackXY_mex(CubeBS, X1, Y1, Args.AperRadus, Args.Annulus, true) %(~0.0006s on 3e4 sources)
        otherwise
            error('Unknwon AperPhotMethod option');
    end

    % Prep data products
    PhotData = [X, Y, X1, Y1, AperPhot, Back, BackStd, AperArea];

    % calculate global shift relative to previous guess position
    [Shift.ShiftX, Shift.StdShiftX] = tools.math.stat.mex.meanStd(X1-XY(:,1));
    [Shift.ShiftY, Shift.StdShiftY] = tools.math.stat.mex.meanStd(Y1-XY(:,2));


end
