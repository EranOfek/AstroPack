function Result=findSourceInCat(List, RA, Dec, Args)
    % find a single source by coordinates in a list of AstroCatalog of FITS tables.
    %       Given FITS files containing AstroCatalog catalogs, or
    %       AstroCatalog objects go over all tables, and look for sources
    %       nearest to a position.
    % Input  : - Either:
    %            AstroCatalog object, AstroImage object containing
    %            AstroCatalog, a char array (with optional wild cards) of
    %            file names to search in the current dir (optionally recursively),
    %            or a cell array of file names.
    %          - J2000.0 RA (deg/rad/sex) or X [pix] coordinate.
    %          - J2000.0 Dec (deg/rad/sex) or Y [pix] coordinate.
    %          * ...,key,val,...
    %            'CooUnits' - Coordinate units for input. Default is 'deg'.
    %            'SearchRadius' - Search radius. Default is 3.
    %            'SearchRadiusUnits' - Default is 'arcsec'.
    %            'Recursive' - Search files recursively in directory tree.
    %                   Default is true.
    %            'IsCooSphere' - A logical indicating if to use spherical
    %                   coordinates. False assuming x/y pix positions are provided.
    %                   Default is true.
    %            'CheckFootprint'
    %            'DefNamesRA' - Column names dictionary for RA.
    %                   Default is AstroCatalog.DefNamesRA
    %            'DefNamesDec' - Column names dictionary for Dec.
    %                   Default is AstroCatalog.DefNamesDec
    %            'DefNamesX' - Column names dictionary for X.
    %                   Default is AstroCatalog.DefNamesX
    %            'DefNamesY' - Column names dictionary for Y.
    %                   Default is AstroCatalog.DefNamesY.
    %            'OutType' - Output option for Table field in output.
    %                   'table' - return a table of selected sources.
    %                   'selected' - return AstroCatalog with selected
    %                           objects.
    %                   'all' - return AstroCatalog with all sources.
    %                   'mat' - return matrix with selected sources.
    % Output : - A structure array (element per AstroCatalog) with the
    %            following fields:
    %            .JD - JD of catalog.
    %            .Nfound - Number of sources found within search radius.
    %            .Dist - Dist [rad] of selected sources from search
    %                   position.
    %            .Ind - Indices of selected sources out of the full
    %                   catalog.
    %            .Table - Output table/list with sources.
    % Author : Eran Ofek (Jan 2023)
    % Example: imProc.cat.findSourceInCat('LAST*_Cat*.fits',50.0452635724,+8.748787337);

    arguments
        List   % AstroCat | AstroImage | cell array of files | char of file names
        RA
        Dec
        Args.CooUnits                = 'deg';
        Args.SearchRadius            = 3;
        Args.SearchRadiusUnits       = 'arcsec';
        Args.Recursive logical       = true; % relevant only for char file names
        Args.IsCooSphere logical     = true; % otherwise [X,Y]
        Args.CheckFootprint logical  = true;
        
        Args.DefNamesRA              = AstroCatalog.DefNamesRA;
        Args.DefNamesDec             = AstroCatalog.DefNamesDec;
        Args.DefNamesX               = AstroCatalog.DefNamesX;
        Args.DefNamesY               = AstroCatalog.DefNamesY;
        Args.OutType                 = 'table';  % 'table' | 'selected' | 'all' | 'mat'

        Args.Verbose logical         = false;
    end

    % convert coordinates to rad
    if ischar(RA) || iscell(RA)
        RA  = celestial.coo.convertdms(RA,  'gH', 'r');
        Dec = celestial.coo.convertdms(Dec, 'gD', 'r');
    else
        Factor = convert.angular(Args.CooUnits, 'rad', 1);
        RA     = RA.*Factor;
        Dec    = Dec.*Factor;
    end

    if Args.IsCooSphere
        SearchRadius = convert.angular(Args.SearchRadiusUnits, 'rad', Args.SearchRadius);
    else
        SearchRadius = Args.SearchRadius;
    end

    if ischar(List)
        if Args.Recursive
            SDir = io.files.rdir(List);
        else
            SDir = dir(List);
        end
        List = fullfile({SDir.folder},{SDir.name});
    end


    
    Nlist = numel(List);
    Result = struct('JD',cell(Nlist,1), 'Nfound',cell(Nlist,1), 'Dist',cell(Nlist,1), 'Ind',cell(Nlist,1), 'Table',cell(Nlist,1));
    for Ilist=1:1:Nlist
        if Args.Verbose
            if mod(Ilist,100)==0
                [Ilist, Nlist]
            end
        end
        
        if isa(List, 'AstroImage')
            Cat = List(Ilist).CatData;
            JD  = List(Ilist).julday;
        elseif isa(List, 'AstroCatalog')
            Cat = List(Ilist);
            JD  = List(Ilist).JD;
        elseif iscell(List)
            Cat = AstroCatalog(List(Ilist));
            JD  = [];
        else
            error('Unknown List type option');
        end

        % Search for source in catalog
        if Args.IsCooSphere
            % search by RA/Dec
            [CatRA, CatDec] = getLonLat(Cat, 'rad', 'ColLon',Args.DefNamesRA, 'ColLat',Args.DefNamesDec);

            Dist = celestial.coo.sphere_dist_fast(RA, Dec, CatRA, CatDec);
            Flag = Dist<SearchRadius;

        else
            % search by pixels x/y
            [CatX, CatY] = getLonLat(Cat, 'ColX',Args.DefNamesX, 'ColY',Args.DefNamesY);

            Dist = tools.math.geometry.plane_dist(RA, Dec, CatX, CatY);
            Flag = Dist<SearchRadius;

        end

        if isempty(JD)
            % attempt to get JD from image name
            if iscell(List)
                JD = FileNames.getValFromFileName(List{Ilist});
            end
        end
        Result(Ilist).JD       = JD;
        Result(Ilist).Nfound   = sum(Flag);
        Result(Ilist).Dist     = Dist(Flag);  % [rad]
        Result(Ilist).Ind      = find(Flag);
        switch Args.OutType
            case 'all'
                Result(Ilist).Table    = Cat;
            case 'selected'
                Result(Ilist).Table    = selectRows(Cat, Flag, 'CreateNewObj',true);
            case 'mat'
                Result(Ilist).Table    = Cat.Catalog(Flag,:);
            case 'table'
                Tmp = selectRows(Cat, Flag, 'CreateNewObj',true);
                Result(Ilist).Table    = Tmp.toTable;
            otherwise
                error('Unknown OutType option');
        end

    end
end
