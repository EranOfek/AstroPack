function [Args] = prepArgsForAstrometry(Obj, Args)
    % Preparing Args for astrometry - this function is a common block of both astrometryRefine and astrometryCore
    %   Update: Args.RA, Dec, Tran, EpochOut
    %   Args.RA/Args.Dec are resolved into numeric coordinates only when they
    %   are not numeric already - i.e. when empty, or when they hold header
    %   keyword names ('RA'/'DEC', the astrometryCore defaults) or sexagesimal
    %   strings. For an AstroImage they are read from the header; for an
    %   AstroCatalog there is no header, so empty values are left empty and the
    %   calling function derives the field center from the catalog itself
    %   (boundingCircle). See issues #1294, #1299.
    % Input  : - Input object.
    %          - Structure of argumnets.
    % Output : - Updated structure of arguments.
    % Author : Eran Ofek (2025 Nov) 
    % Example: Args=imProc.astrometry.prepArgsForAstrometry(Obj, Args);

    arguments
        Obj
        Args
    end

    %
    
    % old - issue #1294
    %if isempty(Args.RA) || isempty(Args.Dec)
    %    if isa(Obj, 'AstroImage')
    %        % can read RA/Dec from Header if AstroImage
    %        [Args.RA, Args.Dec] = getCoo(Obj(1).HeaderData, 'RA',Args.RA, 'Dec',Args.Dec, 'Units',Args.CooUnits, 'OutUnits',Args.CooUnits);
    %    else
    %        [Args.RA, Args.Dec] = celestial.coo.parseCooInput(1, 1, 'InUnits',Args.CooUnits, 'OutUnits',Args.CooUnits);
    %    end
    %end        
    % Non-numeric RA/Dec still have to be resolved: astrometryCore defaults to
    % the header keyword names 'RA'/'DEC', which are not coordinates (#1299).
    if isempty(Args.RA) || isempty(Args.Dec) || ~isnumeric(Args.RA) || ~isnumeric(Args.Dec)
        if isa(Obj, 'AstroImage')
            % Read RA/Dec from the AstroImage header. getCoo resolves keyword
            % names and sexagesimal strings, and returns empty for empty input
            % - astrometryRefine then uses the catalog bounding circle.
            [Args.RA, Args.Dec] = getCoo(Obj(1).HeaderData, ...
                'RA', Args.RA, ...
                'Dec', Args.Dec, ...
                'Units', Args.CooUnits, ...
                'OutUnits', Args.CooUnits);

        elseif isa(Obj, 'AstroCatalog')
            % Keep RA/Dec as they are. There is no header to resolve them
            % against, and astrometryCore/astrometryRefine calculate the field
            % center from the catalog using boundingCircle.

        else
            error('Unsupported input class. First input must be AstroCatalog or AstroImage');
        end
    end


    % make sure Tran is a new copy, otherwise may overwrite other Tran
    Args.Tran = Args.Tran.copy;
    
    % get EpochOut
    if isempty(Args.EpochOut)
        if isa(Obj, 'AstroImage')
            Args.EpochOut = julday(Obj);
            if any(isnan(Args.EpochOut))
                Args.EpochOut = [];
            end
        end
    end


end
