function [Result] = getCooCenter(Obj, Args)
    % Get image/catalog center coordinates and radius
    % Input  : - An AstroImage or AstroCatalog object.
    %          * ...,key,val,... 
    %            'RA' - If 'RA','Dec','FOV_Radius' are given then they will
    %                   be use instead of retrievied from the
    %                   image/catalog.
    %                   Default is [].
    %            'Dec' - Like 'RA', but for Declination.
    %            'FOV_Radius' - Like 'RA', but for FoV radius.
    %            'InCooUnits' - Input Coordinates units.
    %                   This must be provided along with RA, Dec,
    %                   FOV_Radius. Default is [].
    %            'OutCooUnits' - Output coordinates units.
    %                   Default is 'deg'.
    %            'UseWCS' - A logical. If input is AstroImage, then if this
    %                   argument is true, then will use the WCS to measure the
    %                   image center (otherwise use catalog).
    %                   Default is true.
    %            'UpdateHeader' - A logical indicating if to update the
    %                   AstroImage header with the new image center coordinates.
    %                   Default is false.
    %            'KeyRA' - The Keyword name in the header in which to store
    %                   the new calculated RA. Default is 'RA'.
    %            'KeyDec' - The Keyword name in the header in which to store
    %                   the new calculated Dec. Default is 'Dec'.
    % Output : - A 3 column matrix of [RA, Dec, FOV_Radius] for each one of
    %            the input image/catalogs.
    % Author : Eran Ofek (2023 Dec) 
    % Example: Result=imProc.astrometry.getCooCenter(Obj);

    arguments
        Obj
        Args.RA                    = [];
        Args.Dec                   = [];
        Args.FOV_Radius            = [];
        Args.InCooUnits            = [];
        Args.OutCooUnits           = 'deg';
        
        Args.UseWCS logical        = true;  % if false will use catalog

        Args.UpdateHeader logical  = false;
        Args.KeyRA                 = 'RA';
        Args.KeyDec                = 'Dec';
    end

    Nobj   = numel(Obj);
    if ~isempty(Args.RA) && ~isempty(Args.Dec) && ~isempty(Args.FOV_Radius)
        Factor          = convert.angular(Args.InCooUnits, Args.OutCooUnits);
        Ones            = ones(Nobj, 1);
        Args.RA         = Ones.*Factor .* Args.RA;
        Args.Dec        = Ones.*Factor .* Args.Dec;
        Args.FOV_Radius = Ones.*Factor .* Args.FOV_Radius;
        Result          = [Args.RA(:), Args.Dec(:), Args.FOV_Radius(:)];
    else
                
        Result = zeros(Nobj, 3);
        for Iobj=1:1:Nobj

            if isa(Obj(Iobj), 'AstroImage')
                if Args.UseWCS
                    ResCoo = Obj(Iobj).cooImage([], 'OutUnits',Args.OutCooUnits);
                    Result(Iobj,:) = [ResCoo.Center, ResCoo.FOV_Radius];

                    if Args.UpdateHeader
                        Obj(Iobj).HeaderData = replaceVal(Obj(Iobj).HeaderData, {Args.KeyRA, Args.KeyDec}, ResCoo.Center(1:2));
                    end

                else
                    [RA, Dec, FOV_Radius] = boundingCircle(Obj(Iobj).CatData, 'OutUnits',Args.OutCooUnits ,'CooType','sphere');
                    Result(Iobj,:) = [RA, Dec, FOV_Radius];
                end
            else
                % assume Obj is AstroCatalog
                [RA, Dec, FOV_Radius] = boundingCircle(Obj(Iobj), 'OutUnits',Args.OutCooUnits ,'CooType','sphere');
                Result(Iobj,:) = [RA, Dec, FOV_Radius];
            end
        end % end for Iobj loop
    end
end
