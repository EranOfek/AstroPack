function Result = matchReturnIndices(Obj1, Obj2, Args)
    % Match two catalogs in AstroCatalog objects and return the matched indices.
    %       This is a basic utility function that returns the two-directional
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
    % Output : - A structure array (element per Obj1/Obj2 matching) with
    %            the following fields:
    %            'Obj2_IndInObj1' - A vector, for each source in Obj2, of
    %                   its matched indices in Obj1. NaN if no match.
    %            'Obj2_Dist' - A vector, for each source in Obj2, of the
    %                   angular distance ['rad' if 'sphere'] between the
    %                   matched sources.
    %            'Obj2_NmatchObj1' - A vector, for each source in Obj2, of the
    %                   number of matches within search radius.
    %            'Obj1_IndInObj2' - A vector, for each source in Obj1, of 
    %                   its matched indices in Obj2. NaN if no match.
    %            'Obj1_FlagNearest' - A vector, for each source in Obj1,
    %                   of logicals indicating if this source is the neaest
    %                   match to a source is Obj2.
    %            'Obj1_FlagAll' - A vector, for each source in Obj1,
    %                   of logicals indicating if this source is a
    %                   match (within search radius) to a source is Obj2.
    %            'Obj1_Fist' - A vector, for each source in Obj1, of the
    %                   angular distance ['rad' if 'sphere'] between the
    %                   matched sources.
    % Author : Eran Ofek (Sep 2021)
    % Example : AC = AstroCatalog;
    %           AC.Catalog  = [1 0; 1 2; 1 1; 2 -1; 2 0; 2.01 0];
    %           AC.ColNames = {'RA','Dec'}; AC.ColUnits = {'rad','rad'};
    %           AC2 = AstroCatalog; AC2.Catalog  = [1 2; 1 1; 2.001 0; 3 -1; 3 0]
    %           AC2.ColNames = {'RA','Dec'}; AC2.ColUnits = {'rad','rad'};
    %           Result = imProc.match.matchReturnIndices(AC,AC2,'Radius',0.01,'CooType','sphere','RadiusUnits','rad')

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
        
    end    

    if Args.CreateNewObj
        Obj1 = Obj1.copy();
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
    
    Result = struct('Obj2_IndInObj1',cell(Nmax,1), 'Obj2_Dist',cell(Nmax,1), 'Obj2_NmatchObj1',cell(Nmax,1),...
                    'Obj1_IndInObj2',cell(Nmax,1), 'Obj1_FlagNearest',cell(Nmax,1), 'Obj1_FlagAll',cell(Nmax,1));
    for Imax=1:1:Nmax
        Iobj1 = min(Imax, Nobj1);
        Iobj2 = min(Imax, Nobj2);

        if isempty(CommonCooType{Imax})
            error('CooType is not consistent while matching: Iobj1=%d, Iobj2=%d',Iobj1,Iobj2);
        end
        
        % convert AstroImage to AstroCatalog: Obj1
        if isa(Obj1,'AstroImage')
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
                DistFun = @celestial.coo.sphere_dist_fast; %Thresh; 
                
                Coo1    = double(getLonLat(Cat1, 'rad'));
                Coo2    = double(getLonLat(Cat2, 'rad'));

                RadiusRad = convert.angular(Args.RadiusUnits, 'rad', Args.Radius);
                %DistFunArgs{1} = RadiusRad;
                DistFunArgs = {}; %{RadiusRad};
                ConvertDist = true;
            case 'pix'
                DistFun = @tools.math.geometry.plane_dist;
                %DistFunArgs = {};
                Coo1    = double(getXY(Cat1));
                Coo2    = double(getXY(Cat2));

                RadiusRad = Args.Radius;
                DistFunArgs = {};
                ConvertDist = false;
            otherwise
                error('Unknown CooType option');
        end   

        % match
        [IndTable, CatFlagNearest, CatFlagAll, IndInObj2] = VO.search.search_sortedlat_multiNearest(Coo1,...
                                                    Coo2(:,1), Coo2(:,2), RadiusRad, DistFun, DistFunArgs);

        % Columns of IndTable:
        % For each source in Obj2:
        % 1. Index of nearest source, within search radius, in Obj1
        % 2. Distance;
        % 3. Total number of matches within radius.
        Result(Imax).Obj2_IndInObj1   = IndTable(:,1);
        Result(Imax).Obj2_Dist        = IndTable(:,2);
        Result(Imax).Obj2_NmatchObj1  = IndTable(:,3);
        % IndInRef: For each source in Obj1 its index in Obj2
        Result(Imax).Obj1_IndInObj2   = IndInObj2;
        Result(Imax).Obj1_FlagNearest = CatFlagNearest;
        Result(Imax).Obj1_FlagAll     = CatFlagAll;

        %Result(Imax).Obj1_Dist = nan(size(CatFlagAll));
        FlagNaN = isnan(IndInObj2);
        IndInObj2(FlagNaN) = 1;  % temporary
        
        % new: Obj1_Dist
        Col = 2;
        Result(Imax).Obj1_Dist          = IndTable(IndInObj2, Col);
        Result(Imax).Obj1_Dist(FlagNaN) = NaN;
    end
end
