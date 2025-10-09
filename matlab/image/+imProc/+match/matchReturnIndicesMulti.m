function Result = matchReturnIndicesMulti(Obj1, Obj2, Args)
    % Match two catalogs in AstroCatalog objects, and for each source in Obj2, return all the matched sources in Obj1.
    %       This is a basic utility function that returns the
    %       indices of the matched sources.
    %       This function is used by the more advanced matching programs.
    % Input  : - An AstroCatalog/AstroImage object.
    %            If multiple elements then each element will be
    %            matched against the corresponding element (or a
    %            single element) in the second object. 
    %            If this object is not sorted, then the object will be
    %            sorted (and modified, unlse CreateNewObj=true).
    %          - A second AstroCatalog object - The function will
    %            attempt to match every source in this catalog with
    %            objects in the first input catalog.
    %          * ..., key, val,..
    %            'Radius'  - Search radius. Default is 5.
    %            'RadiusUnits' - Search radius units (if spherical
    %                   coordinates search). Default is 'arcsec'.
    %            'CreateNewObj' - A logical indicating if to create a new
    %                   copy of Obj1 if it is not sorted. If false, then
    %                   Obj1 will be modified. Default is false.
    %            'CooType' - CooType (i.e., 'pix','sphere').
    %                   If empty, will use what is available in the catalog
    %                   with preference for 'sphere'. Default is empty.
    %            'ColCatX' - If CooType is not empty, this is the column
    %                   names/index from which to select the catalog X
    %                   coordinate. Default is [].
    %            'ColCatY' - Like 'ColCatX', but for the Y coordinate.
    %            'ColRefX' - Like 'ColCatX', but for te ref catalog.
    %            'ColRefY' - Like 'ColRefX', but for the Y coordinate.
    %            'CalcDist' - Return also the distances to the matched
    %                   sources and the distance to the nearest source.
    %                   Default is true;
    %
    % Output : - A structure array (element per Obj1/Obj2 matching) 
    %            the .Ind field.
    %            By itself this is a structure array that returned by:
    %            VO.search.search_sortedlat_multi
    %            The size of the array is like that of the catalog in the
    %            second input.
    % Author : Eran Ofek (Sep 2021)
    % Example : AC = AstroCatalog;
    %           AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
    %           AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};
    %           AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0]
    %           AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
    %           Result = imProc.match.matchReturnIndicesMulti(AC,AC2,'Radius',0.01,'CooType','sphere','RadiusUnits','rad')

    arguments
        Obj1
        Obj2
        Args.Radius                      = 5;
        Args.RadiusUnits                 = 'arcsec';
        % if given will override ColX/ColY
        Args.CooType                     = 'pix';   % MUST BE SPECIFIED: 'pix' | 'sphere'
        Args.ColCatX                     = [];
        Args.ColCatY                     = [];
        Args.ColRefX                     = [];
        Args.ColRefY                     = [];
        Args.CreateNewObj(1,1) logical   = false; % for the sorted version of Obj1
        
        Args.SphereDistFun               = @celestial.coo.sphere_dist_fast; %@celestial.coo.sphere_dist_fast_threshDist; %Thresh;
        Args.SphereDistFunArgs           = {}; %{4.8481e-5};

        Args.CalcDist                    = true;
    end    

    if Args.CreateNewObj
        Obj1 = Obj1.copy();
    end

    if Args.CalcDist
        CalcDist = -1;
    else
        CalcDist = 1;
    end

    Nobj1 = numel(Obj1);
    Nobj2 = numel(Obj2);
    Nmax  = max(Nobj1, Nobj2);

    % select CooType
    if isempty(Args.CooType)
        % attempt to select automatically
        [~, ~, CommonCooType] = getCommonCooType(Obj1, Obj2);
    else
        [CommonCooType{1:Nmax}] = deal(Args.CooType);
    end
    
    Result = struct('Ind',cell(Nmax,1));
    for Imax=1:1:Nmax
        Iobj1 = min(Imax, Nobj1);
        Iobj2 = min(Imax, Nobj2);

        if isempty(CommonCooType{Imax})
            error('CooType is not consistent while matching: Iobj1=%d, Iobj2=%d',Iobj1,Iobj2);
        end
        
        % convert AstroImage to AstroCatalog: Obj1
        if isa(Obj1,'AstroImage') || isa(Obj1, 'AstroDiff') || isa(Obj1, 'AstroZOGY')
            Cat1 = Obj1(Iobj1).CatData;
        elseif isa(Obj1,'AstroCatalog')
            Cat1 = Obj1(Iobj1);
        elseif isnumeric(Obj1(Iobj1))
            error('Input Obj1 is of unsupported class');
        else
            error('Input Obj1 is of unsupported class');
        end

         % convert AstroImage to AstroCatalog: Obj2
        if isa(Obj2,'AstroImage')
            Cat2 = Obj2(Iobj2).CatData;
        elseif isa(Obj2,'AstroCatalog')
            Cat2 = Obj2(Iobj2);
        elseif isnumeric(Obj2(Iobj2))
            error('Input Obj2 is of unsupported class');
        else
            error('Input Obj2 is of unsupported class');
        end

        if ~Cat1.IsSorted
            [~, ColY] = getColCooForCooType(Cat1, Args.CooType);
            Cat1.sortrows(ColY);
        end

        switch lower(CommonCooType{Imax})
            case 'sphere'
                %DistFun = @celestial.coo.sphere_dist_fast; %Thresh; 
                DistFun = Args.SphereDistFun; %@celestial.coo.sphere_dist_fast_threshDist; %Thresh;
                DistFunArgs = Args.SphereDistFunArgs; % {4.8481e-5};

                Coo1    = double(getLonLat(Cat1, 'rad'));
                Coo2    = double(getLonLat(Cat2, 'rad'));

                RadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.Radius);
                %DistFunArgs{1} = RadiusRad;
                
                ConvertDist = true;
            case 'pix'
                DistFun = @tools.math.geometry.plane_dist;
                DistFunArgs = {};
                Coo1    = double(getXY(Cat1));
                Coo2    = double(getXY(Cat2));

                RadiusRad = Args.Radius;
                %DistFunArgs = {};
                ConvertDist = false;
            otherwise
                error('Unknown CooType option');
        end   

        % match
        % CalcDist is either +1 or -1, depanding if we ant to get out the
        % distances
        [Result(Imax).Ind]=VO.search.search_sortedlat_multi(Coo1,...
                                                    Coo2(:,1), Coo2(:,2), CalcDist.*RadiusRad, [], DistFun, 'DistFunArgs',DistFunArgs);
        

    end
end
