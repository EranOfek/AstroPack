function [Result] = aperPhot(Obj, Args)
    % Perform aperture photometry on a predefined list of positions
    %     Given an AstroImage object and a list of positions, calculate the
    %     aperture photometry of the sources. THe Flux, FluxErr, Mag,
    %     MagErr in all aperture is added/replacing to the catalog of the
    %     image.
    % Input  : - An AstroImage object.
    %          * ...,key,val,... 
    %            'XY' - An optional two column matrix of [X, Y] pixel
    %                   positions, in which to measure the aperture photometry.
    %                   If empty, then will attempt to read the positions
    %                   from the AstroCatalog object.
    %                   Default is [].
    %            'RadiusStamp' - Photometrry stamp half radius.
    %                   Default is 12.
    %            'mexCutout' - Use mex cutout. Default is true.
    %            'Circle' - Use circle cutouts. Default is false.
    %            'AperRadius' - Photometric aperture radius [pix].
    %                   Default is [2 4 6].
    %            'Annulus' - Inner/outer radius of background annulus.
    %                   Default is [10 12].
    %            'ZP' - Photometric ZP for magnitude/luptitude.
    %                   Default is 25.
    %            'ColFlux' - Prefix of flux columns.
    %                   Default is 'FLUX_APER'.
    %            'ColFluxErr' - Prefix of flux error columns.
    %                   Default is 'FLUX_APER'.
    %            'ColMag' - Prefix of mag columns.
    %                   Default is 'MAG_APER'.
    %            'ColMagErr' - Prefix of mag error columns.
    %                   Default is 'MAGERR_APER'.
    %            'CreateNewObj' - Create new object. Default is false.
    % Output : - AN AstroImage with the updated catalog with the aperture
    %            photometry data.
    % Author : Eran Ofek (2025 Dec) 
    % Example: R=imProc.sources.aperPhot(AI);

    arguments
        Obj
        Args.XY                = [];
        Args.RadiusStamp       = 12;
        Args.mexCutout         = true;
        Args.Circle            = false;

        Args.AperRadius        = [2 4 6];
        Args.Annulus           = [10 12];

        Args.ZP                = 25;

        Args.ColFlux           = 'FLUX_APER';
        Args.ColFluxErr        = 'FLUXERR_APER';
        Args.ColMag            = 'MAG_APER';
        Args.ColMagErr         = 'MAGERR_APER'
        Args.ColXY             = {'X','Y'};


        Args.CreateNewObj      = false;
    end


    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    % column names
    Naper      = numel(Args.AperRadius);
    ColFlux    = tools.cell.cellNumericSuffix(Args.ColFlux, (1:Naper));
    ColFluxErr = tools.cell.cellNumericSuffix(Args.ColFluxErr, (1:Naper));
    ColMag     = tools.cell.cellNumericSuffix(Args.ColMag, (1:Naper));
    ColMagErr  = tools.cell.cellNumericSuffix(Args.ColMagErr, (1:Naper));
    ColsToAdd  = [ColFlux, ColFluxErr, ColMag, ColMagErr];
    [C1{1:Naper.*2}] = deal('');
    [C2{1:Naper.*2}] = deal('mag');
    ColUnits         = [C1, C2];


    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isempty(Args.XY)
            % read XY positions from AstroCatalog in AstroImage
            XY = Result(Iobj).CatData.getXY();
        else
            XY = Args.XY;
            % replace current AStroCatalog
            Result(Iobj).CatData = AstroCatalog({XY}, 'ColNames',Args.ColXY);
        end

        [Cube, RoundX, RoundY, X, Y] = imUtil.cut.image2cutouts(Result(Iobj).ImageData.Image, XY(:,1), XY(:,2), Args.RadiusStamp, 'mexCutout',Args.mexCutout, 'Circle',false);
            
        ResAperBright = imUtil.sources.aperPhotCube(Cube, 'AperRad',Args.AperRadius, 'AnnulusRad',Args.Annulus);

        
        FluxMagData = [ResAperBright.AperPhot,...
                       ResAperBright.AperPhotErr,...
                       convert.luptitude(ResAperBright.AperPhot, 10.^(0.4.*Args.ZP)),...
                       1.086.*ResAperBright.AperPhotErr./ResAperBright.AperPhot];

        
        Result(Iobj).CatData.replaceCol(double(FluxMagData), ColsToAdd, Inf, ColUnits);
        %Result(Iobj).CatData.insertCol(FluxMagData, Inf, ColsToAdd, ColUnits);

    end



end
