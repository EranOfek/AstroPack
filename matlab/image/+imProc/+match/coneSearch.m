function [Result, Flag, AllDist] = coneSearch(CatObj, Coo, Args)
    % cone search(s) on AstroCatalog/AstroImage object
    % Input  : - An AstroCatalog/AstroImage object. If multiple elements
    %            then will perform the cone search on any one of
    %            the elements.
    %          - A two column matrix of [RA, Dec] or [X, Y]
    %            to search.
    %            If more then one row, then the results of the
    %            search will be merged.
    %          * ...,key,val,...
    %            'CooType' - Which Coo system to use 'pix' (for X/Y),
    %                   or 'sphere' (for RA/Dec).
    %                   If empty, will look for 'sphere', and if not exist
    %                   will use 'pix'. Default is [].
    %            'Radius'  - Search radius. Default is 5.
    %            'RadiusUnits' - Search radius units (if spherical
    %                   coordinates search). Default is 'arcsec'.
    %            'Shape' - Shape search. Default is 'circle'.
    %            'CooUnits' - Units of (spherical) coordinates
    %                   (second input argument). Default is 'deg'.
    %            'AddDistCol' - Add column distance to outout
    %                   catalog. Default is true.
    %            'DistUnits' - Distance units. Default is 'arcsec'.
    %            'DistColName' - Distance column name.
    %                   Default is 'Dist'.
    %            'DistColPos' - Position of Distance column in
    %                   output catalog. Default is Inf (i.e., last
    %                   column).
    %            'CreateNewObj' - Indicating if the output
    %                   is a new copy of the input (true), or an
    %                   handle of the input (false).
    %                   If empty (default), then this argument will
    %                   be set by the number of output args.
    %                   If 0, then false, otherwise true.
    %                   This means that IC.fun, will modify IC,
    %                   while IB=IC.fun will generate a new copy in
    %                   IB.
    % Output : - An AstroCatalog object with the found sources.
    %          - A vector of logicals with the same length as the
    %            number of rows in the input object. True if object
    %            is in cone. If the input is an object with
    %            multiple elements, then this vector corresponds to
    %            the last element in the object array.
    %          - A vector of distances of the found objects from
    %            the cone search center. If the input is an object with
    %            multiple elements, then this vector corresponds to
    %            the last element in the object array.
    % Author : Eran Ofek (Apr 2021)
    % Example: AC=AstroCatalog({'asu.fit'},'HDU',2);
    %          [NC, Flag, Dist] = imProc.match.coneSearch(AC,[1 1],'Radius',3600)
    %          [NC, Flag, Dist] = imProc.match.coneSearch(AC,[1 1; 0 0],'Radius',3600);  % search around two positions (merged results).

    arguments
        CatObj
        Coo
        Args.CooType                     = [];
        Args.Radius                      = 5;
        Args.RadiusUnits                 = 'arcsec';
        Args.Shape char                  = 'circle';
        Args.CooUnits char               = 'deg';
        Args.AddDistCol                  = true;
        Args.DistUnits                   = 'arcsec';
        Args.DistColName                 = 'Dist';
        Args.DistColPos                  = Inf;
        Args.CreateNewObj                = [];
    end

%     if isempty(CatObj(1).CooType)
%         CatObj.getCooTypeAuto;
%     end

    % Convert Coo to radians
    CooRad = convert.angular(Args.CooUnits,'rad',Coo);

    if isempty(Args.CreateNewObj)
        if nargout>0
            Args.CreateNewObj = true;
        else
            Args.CreateNewObj = false;
        end
    end

    % convert AstroImage to AstroCatalog
    if isa(CatObj,'AstroImage')
        CatObj = astroImage2AstroCatalog(CatObj,'CreateNewObj',Args.CreateNewObj);
    elseif isa(CatObj,'AstroCatalog')
        % do nothing
    elseif isnumeric(CatObj)
        error('Input CatObj is of unsupported class');
    else
        error('Input CatObj is of unsupported class');
    end

    RadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.Radius);
    if Args.AddDistCol || nargout>2
        RadiusRad = -abs(RadiusRad);
    end

    if Args.CreateNewObj
        Result = CatObj.copy();
		Result.Catalog = [];
    else 
        Result = CatObj;
    end

    SizeCat = sizeCatalog(CatObj);
    
    Nobj = numel(CatObj);
    for Iobj=1:1:Nobj
        if SizeCat(Iobj)>0   % otherwise empty - skip
            
            if isempty(Args.CooType)
                [CooType, ~, ColX, ColY] = getCooType(CatObj(Iobj));
                CooType = CooType{1};
            else
                CooType = Args.CooType;
                [ColX, ColY] = getColCooForCooType(CatObj(Iobj), CooType);
            end

            if ~CatObj(Iobj).IsSorted
                CatObj(Iobj).sortrows(ColY);
            end

            switch lower(CooType)
                case 'sphere'
                    [Ind,Flag] = VO.search.search_sortedlat_multi(getLonLat(CatObj(Iobj),'rad'),...
                                                                CooRad(:,1), CooRad(:,2), RadiusRad, [],...
                                                                @celestial.coo.sphere_dist_fast);
                case 'pix'
                    [Ind,Flag] = VO.search.search_sortedlat_multi(getXY(CatObj(Iobj)),...
                                                                Coo(:,1), Coo(:,2), RadiusRad, [],...
                                                                @tools.math.geometry.plane_dist);

                otherwise
                    error('Unknown CooType option');
            end


            % what to do with the found objects
            Ncoo = numel(Ind);
            %Out     = zeros(0, size(CatObj(Iobj).Catalog,2));
            AllDist = zeros(0,1);
            for Icoo=1:1:Ncoo
                if Icoo==1
                    Out = CatObj(Iobj).Catalog(Ind(Icoo).Ind,:);
                else
                    Out     = [Out; CatObj(Iobj).Catalog(Ind(Icoo).Ind,:)];
                end
                AllDist = [AllDist; Ind(Icoo).Dist]; 
            end
            AllDist  = convert.angular('rad', Args.DistUnits, AllDist);
            Result(Iobj).Catalog = Out;
            if Args.AddDistCol
                Result(Iobj).insertCol(AllDist, Args.DistColPos, Args.DistColName, Args.DistUnits);
            end
        end
    end
end
