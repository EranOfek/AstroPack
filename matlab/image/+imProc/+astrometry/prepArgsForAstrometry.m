function [Args] = prepArgsForAstrometry(Obj, Args)
    % Preparing Args for astrometry - this function is a common block of both astrometryRefine and astrometryCore
    %   Update: Args.RA, Dec, Tran, EpochOut
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
    if isempty(Args.RA) || isempty(Args.Dec)
      if isa(Obj, 'AstroImage')
          % Read RA/Dec from the AstroImage header.
          [Args.RA, Args.Dec] = getCoo(Obj(1).HeaderData, ...
              'RA', Args.RA, ...
              'Dec', Args.Dec, ...
              'Units', Args.CooUnits, ...
              'OutUnits', Args.CooUnits);

      elseif isa(Obj, 'AstroCatalog')
          % Keep RA/Dec empty. astrometryRefine will calculate the field
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
