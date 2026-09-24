function [Result] = match_catsHTM_multi(Obj, Args)
    % Match an AstroCatalog (in AstroImage) with multiple catsHTM catalogs with flexible criteria and add information to the catalog.
    %     Given an AstroCatalog (or catalog in AstroImage) match all the
    %     sources against a list of catsHTM catalogs.
    %     Add new columns with the matched information to the input
    %     catalog.
    %     For each catsHTM catalog, the matching details are flexible and
    %     can be controlled via the 'Cat' argument (see below).
    %     For example the matching radius can be calculated dynamically
    %     using information in the catalog, and the user can select
    %     specific columns that will be writen to the input catalog.
    % Input  : - An AstroCatalog/AstroImage/AstroZOGY object. 
    %          * ...,key,val,... 
    %            'Cat' - A structure array with information which catsHTM catalogs
    %                   to match. One element per catalog.
    %                   An example for such a structure is in the code.
    %                   If [], then a default search (see code) will be
    %                   applied.
    %                   The structure fields are:
    %                   .CatName - catsHTM catalog name (e.g., 'PGC')
    %                   .Con - A cell array of constraings (see
    %                       catsHTM.cone_search).
    %                    .SearchRadius - Either a numeric scalar of search
    %                       radius in arcsec, or a cell array of column
    %                       names (in the corresponding catsHTM catalog).
    %                       If these are column names then they can be used
    %                       to calculate the actual search radius per
    %                       target, using the function in .FunA.
    %                    .MaxSearchRad - The maximum radius in which to
    %                           look for counterparts. This is suseful ehn
    %                           the matched sources are large (e.g.,
    %                           galaxies) and several objects may be tested.
    %                    .UseMinRatio - If true, select the nearest match by
    %                           minimizing distance divided by dynamic SearchRadius
    %                           (dimensionless). If false, select by minimizing
    %                           the absolute distance.
    %                   .FunA - A function handle used to convert the values
    %                           provided in .SearchRadius (from the external
    %                           catalog columns) into a scalar search radius
    %                           per target, in arcsec. 
    %                           If empty, then SearchRadius is scalar.
    %                           Default is [].
    %                   .FunE - Optional function handle for ellipticity/
    %                           axis-ratio based matching (if used). Default is [].
    %                   .FunPA - Optional function handle for position-angle
    %                           based matching (if used). Default is [].
    %                   .Cols - Cell array of column names to extract from
    %                           the external catalog and write into the input
    %                           catalog. Default is {}.
    %                   .ColsUnits - Cell array of units for .Cols, same
    %                           order and length as .Cols. Default is {}.
    %                   .ColsPrefix - A string prefix to prepend to the
    %                           output column names of this specific catsHTM catalog
    %                           (e.g., 'pgc_'). Default is ''.
    %                   .ApplyPM - Logical indicating if to apply proper
    %                           motion in the matching (when supported by the
    %                           catalog). Default is false.
    %                   .KeyJD - Observation mid-time keyword (or empty) to
    %                           use when applying proper motion. Default is [].
    %            'CooUnits' - Coordinate units for the input ('deg'|'rad').
    %                   Default is 'deg'.
    %            'RadiusUnits' - Units for .MaxSearchRad and for the cone
    %                   searches ('arcsec'|'deg'|'rad'). Default is 'arcsec'.
    %            'CheckIsSorted' - If true, verify and sort the input catalog
    %                   by Args.SortCol for faster cone searches.
    %                   Default is false.
    %            'SortCol' - Column name to sort by if CheckIsSorted is true.
    %                   Default is 'Dec'.
    %            'InsertCols' - If true, insert the matched columns into the
    %                   input catalog. If false, compute but do not insert.
    %                   Default is true.
    %            'CreateNewObj' - If true, xreate a deep copy of the object
    %                   before modified.
    % Output : - The input object updated with the new matched columns for
    %            each requested external catalog (or a copy if CreateNewObj
    %            is true). For each catalog, the following columns are added:
    %            <prefix>Dist  [arcsec]  - Distance to the selected match.
    %            <prefix>Nmatch          - Number of candidates returned by
    %                                     the cone search within .MaxSearchRad.
    %            <prefix><Cols>          - The user requested columns from
    %                                     the external catalog (as listed in
    %                                     .Cols) with units specified by
    %                                     .ColsUnits.
    %
    % Author : Eran Ofek (2025 Oct) 
    % Example: AI=imProc.match.match_catsHTM_multi(AI)

    arguments
        Obj
        Args.Cat               = [];
        Args.CooUnits          = 'deg';
        
        Args.CheckIsSorted     = false;
        Args.SortCol           = 'Dec';
        Args.InsertCols        = true;

        Args.CreateNewObj      = false;
    end
    Args.RadiusUnits       = 'arcsec';
    RAD = 180./pi;
    ARCSEC_DEG = 3600;
    ARCSEC_RAD = RAD.*ARCSEC_DEG;

    if Args.CreateNewObj
        Result = Obj.copy;
    else
        Result = Obj;
    end

    if isempty(Args.Cat)
        Ic=1;
        Args.Cat(Ic).CatName      = 'PGC';
        Args.Cat(Ic).Con          = [];
        Args.Cat(Ic).SearchRadius = {'LogD25'}; %,'LogAxisRatio','PA1950'};
        Args.Cat(Ic).MaxSearchRad = 900;
        Args.Cat(Ic).UseMinRatio  = true;   % if true use min(dist in units of SearchRadius); false use min(Dist)
        Args.Cat(Ic).FunA         = @(SearchRadius) 6.*10.^SearchRadius(:,1); % [arcsec]
        Args.Cat(Ic).FunE         = [];
        Args.Cat(Ic).FunPA        = [];
        Args.Cat(Ic).Cols         = {};
        Args.Cat(Ic).ColsUnits    = {};
        Args.Cat(Ic).ColsPrefix   = 'pgc_';
        Args.Cat(Ic).ApplyPM      = false;
        Args.Cat(Ic).KeyJD        = [];
        %Args.Cat(Ic).Multi        = true;

        Ic = Ic + 1;
        Args.Cat(Ic).CatName      = 'GAIADR3extraGal';
        Args.Cat(Ic).Con          = [];
        Args.Cat(Ic).SearchRadius = 10; %{'Rad_Sersic'};
        Args.Cat(Ic).MaxSearchRad = 10;
        Args.Cat(Ic).UseMinRatio  = false;   % if true use min(dist in units of SearchRadius); false use min(Dist)
        Args.Cat(Ic).FunA         = [];
        Args.Cat(Ic).FunE         = [];
        Args.Cat(Ic).FunPA        = [];
        Args.Cat(Ic).Cols         = {'ProbGal','ProbQSO','Rad_Sersic'};
        Args.Cat(Ic).ColsUnits    = {'','','arcsec'};
        Args.Cat(Ic).ColsPrefix   = 'gaiaext_';
        Args.Cat(Ic).ApplyPM      = false;
        Args.Cat(Ic).KeyJD        = [];
        %Args.Cat(Ic).Multi        = false;

        % Ic = Ic + 1;
        % Args.Cat(Ic).CatName      = 'GLADEp';
        % Args.Cat(Ic).Con          = [];
        % Args.Cat(Ic).SearchRadius = {'dL'};
        % Args.Cat(Ic).MaxSearchRad = 20;
        % Args.Cat(Ic).UseMinRatio  = true;   % if true use min(dist in units of SearchRadius); false use min(Dist)
        % Args.Cat(Ic).FunA         = @(SearchRadius) 15./(SearchRadius(:,1).*3600.*180./pi); % [arcsec] @ 15 kpc
        % Args.Cat(Ic).FunE         = [];
        % Args.Cat(Ic).FunPA        = [];
        % Args.Cat(Ic).Cols         = {'zcmb','dL','Bmag'};
        % Args.Cat(Ic).ColsUnits    = {'','Mpc','mag'};
        % Args.Cat(Ic).ColsPrefix   = 'gladep_';
        % Args.Cat(Ic).ApplyPM      = false;
        % Args.Cat(Ic).KeyJD        = [];
        % %Args.Cat(Ic).Multi        = false;

        Ic = Ic + 1;
        Args.Cat(Ic).CatName      = 'GAIADR3var';
        Args.Cat(Ic).Con          = [];
        Args.Cat(Ic).SearchRadius = 3;
        Args.Cat(Ic).MaxSearchRad = 3;
        Args.Cat(Ic).UseMinRatio  = false;   % if true use min(dist in units of SearchRadius); false use min(Dist)
        Args.Cat(Ic).FunA         = [];
        Args.Cat(Ic).FunE         = [];
        Args.Cat(Ic).FunPA        = [];
        Args.Cat(Ic).Cols         = {};
        Args.Cat(Ic).ColsUnits    = {};
        Args.Cat(Ic).ColsPrefix   = 'gaiavar_';
        Args.Cat(Ic).ApplyPM      = false;
        Args.Cat(Ic).KeyJD        = [];
        %Args.Cat(Ic).Multi        = false;

        Ic = Ic + 1;
        Args.Cat(Ic).CatName      = 'QSO1M';
        Args.Cat(Ic).Con          = [];
        Args.Cat(Ic).SearchRadius = 3;
        Args.Cat(Ic).MaxSearchRad = 3;
        Args.Cat(Ic).UseMinRatio  = false;   % if true use min(dist in units of SearchRadius); false use min(Dist)
        Args.Cat(Ic).FunA         = [];
        Args.Cat(Ic).FunE         = [];
        Args.Cat(Ic).FunPA        = [];
        Args.Cat(Ic).Cols         = {'Bmag','z'};
        Args.Cat(Ic).ColsUnits    = {'mag',''};
        Args.Cat(Ic).ColsPrefix   = 'qso_';
        Args.Cat(Ic).ApplyPM      = false;
        Args.Cat(Ic).KeyJD        = [];
        %Args.Cat(Ic).Multi        = false;

    end


    Ncat = numel(Args.Cat);
    Nobj = numel(Result);

    for Iobj=1:1:Nobj
        if isa(Obj, 'AstroCatalog')
            InCat = Result(Iobj);
        else
            InCat = Result(Iobj).CatData;
        end

        if Args.CheckIsSorted
            if ~InCat.isSorted(Args.SortCol)
                InCat.sortrows(Args.SortCol);
            end
        end
            

        for Icat=1:1:Ncat
            %if Args.Cat(Icat).Multi
            % Match to multiple object in external catalog
            [ResInd,Cat] = imProc.match.matchMulti_catsHTM(Result, Args.Cat(Icat).CatName, 'Coo',[],...
                                                   'CooUnits',Args.CooUnits,...
                                                   'Radius',Args.Cat(Icat).MaxSearchRad,...
                                                   'RadiusUnits',Args.RadiusUnits,...
                                                   'Con',Args.Cat(Icat).Con,...
                                                   'CheckIsSorted',false,...
                                                   'catsHTMisRef',false,...
                                                   'ApplyPM',Args.Cat(Icat).ApplyPM,...
                                                   'KeyJD',Args.Cat(Icat).KeyJD);

            % 
            Nsrc = numel(ResInd(Iobj).Ind);
            NcolAdd       = numel(Args.Cat(Icat).Cols);
            if NcolAdd>0
                ExtractedCols = Cat.getCol(Args.Cat(Icat).Cols);
            end
            
            AddCols   = nan(Nsrc,NcolAdd);
            MatchDist = nan(Nsrc,1);
            Nmatch    = nan(Nsrc,1);
            for Isrc=1:1:Nsrc
                %Isrc
                % search for matched source - typicall small search radius
                
                AllInd = ResInd(Iobj).Ind(Isrc).Ind;

                if ~isempty(AllInd)

                    if isempty(Args.Cat(Ic).FunA)
                        SearchRadius = Args.Cat(Icat).SearchRadius;
                    else
                        SearchRadPar = Cat.getCol(Args.Cat(Ic).SearchRadius, 'SelectRows',AllInd);
                        SearchRadius = Args.Cat(Icat).FunA(SearchRadPar);
                    end

                    % distance between sources in units of the SearchRadius
                    if Args.Cat(Icat).UseMinRatio
                        DistInRadiusUnits = ResInd(Iobj).Ind(Isrc).Dist./(SearchRadius./ARCSEC_RAD);
                        [~,Ind1] = min(DistInRadiusUnits);
                        MatchDist(Isrc) = ResInd(Iobj).Ind(Isrc).Dist(Ind1);
                    else
                        % use aboslute distance
                        [MatchDist(Isrc),Ind1] = min(ResInd(Iobj).Ind(Isrc).Dist);  % dist in [rad]
                    end
                    IndNearest = ResInd(Iobj).Ind(Isrc).Ind(Ind1);
                    
                    Nmatch(Isrc)    = ResInd(Iobj).Ind(Isrc).Nmatch;
                    if NcolAdd>0
                        AddCols(Isrc,:) = ExtractedCols(IndNearest,:);
                    end
                    
                end % if ~isempty(AllInd)
            end % for Isrc=1:1:Nsrc

            if Args.InsertCols
                ColNames = ['Dist', 'Nmatch', Args.Cat(Icat).Cols];
                ColNames = tools.cell.cellstr_prefix(ColNames, Args.Cat(Icat).ColsPrefix);
                ColUnits = ['arcsec',{''}, Args.Cat(Icat).ColsUnits];
               
                MatchDist = MatchDist.*ARCSEC_RAD;  % [arcsec]
                InCat.replaceCol([MatchDist, Nmatch, AddCols], ColNames, Inf, ColUnits);
           
            end

        end %for Icat=1:1:Ncat

        if isa(Result, 'AstroCatalog')
            Result(Iobj) = InCat;
        else
            Result(Iobj).CatData = InCat;
        end
    end %for Iobj=1:1:Nobj

end
