function Obj = addMagCols(Obj, Args)
    % Given flux columns in AstroCatalog/AstroImage catalog, add mag and mag err columns.
    % Input  : - An AstroImage or AstroCatalog object containing catalog of
    %            sources, with flux data.
    %          * ...,key,val,...
    %            'FluxCols' - A cell array of column names that contains
    %                   flux measurments.
    %                   Alternatively, a char array that contains a
    %                   substring by which to search for all the flux
    %                   columns.
    %                   Default is 'FLUX_'.
    %            'MagStr' - A cell array of columns names that will
    %                   contains the magnitudes that corresponds to the
    %                   flux in the 'FluxCols' argument.
    %                   Alternatively, a char array that contains a string
    %                   that will replace the corresponding string in
    %                   'FluxCols' argument.
    %                   Default is 'MAG_'.
    %            'FluxErrCols' - Like 'FluxCols' but for flux errors.
    %                   Default is 'FLUXERR_'.
    %            'MagErrStr' - Like 'MagStr' but for magnitude errors.
    %                   Default is 'MAGERR_'.
    %            'ZP' - Magnitude zero point. Default is 25.
    %            'IsLuptitude' - A logical indicating if to calculate
    %                   luptitudes (true) or magnitudes (false).
    %                   Default is true.
    %            'LupSoft' - Luptitude softening parameter.
    %                   see convert.luptitude for details.
    %                   Default is 1e-10.
    % Output : - An AstroImage or AstroCatalog with the added magnitude
    %            columns.
    % Author : Eran Ofek (Jul 2022)
    
    arguments
        Obj      % AstroImage or AstroCatalog
        Args.FluxCols             = 'FLUX_';
        Args.MagStr               = 'MAG_';
        Args.FluxErrCols          = 'FLUXERR_';
        Args.MagErrStr            = 'MAGERR_';
        
        Args.ZP                   = 25;
        Args.IsLuptitude logical  = true;
        Args.LupSoft              = 1e-10;
    end
    
    if ~ischar(Args.FluxCols)
        FluxCols = Args.FluxCols;
        MagCols  = strrep(FluxCols, Args.FluxCols, Args.MagStr);
    end
    if ~ischar(Args.FluxErrCols)
        FluxErrCols = Args.FluxErrCols;
        MagErrCols  = strrep(FluxErrCols, Args.FluxErrCols, Args.MagErrStr);
    end
    
    
    Nobj = numel(Obj);
    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroImage')
            Cat = Obj(Iobj).CatData;
        elseif isa(Obj, 'AstroCatalog')
            Cat = Obj(Iobj);
        else
            error('First input argument must be an AstroImage or AstroCatalog object');
        end
        
        if ischar(Args.FluxCols)
            % search for substring
            Flag        = contains(Cat.ColNames, Args.FluxCols);
            FluxCols    = Cat.ColNames(Flag);
            MagCols     = strrep(FluxCols, Args.FluxCols, Args.MagStr);
        end
        if ischar(Args.FluxErrCols)
            % search for substring
            Flag        = contains(Cat.ColNames, Args.FluxErrCols);
            FluxErrCols = Cat.ColNames(Flag);
            MagErrCols  = strrep(FluxErrCols, Args.FluxErrCols, Args.MagErrStr);
        end
        
        [FluxData,~,FluxColsInd]       = getCol(Cat, FluxCols);
        [FluxErrData,~,FluxErrColsInd] = getCol(Cat, FluxErrCols);
        
        
        % convert flux to magnitude
        if Args.IsLuptitude
            % save luptitude
            MagData = convert.luptitude(FluxData, 10.^(0.4.*Args.ZP), Args.LupSoft);
        else
            MagData = Args.ZP - 2.5.*log10(FluxData);
        end
        % mag errors
        MagErrData = 1.086.* FluxErrData./FluxData;
        
        % insert/replace mag columns:
        Cat = replaceCol(Cat, MagData, MagCols);
        Cat = replaceCol(Cat, MagErrData, MagErrCols);
        
    end        
end