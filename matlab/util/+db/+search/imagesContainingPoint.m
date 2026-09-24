function [Result] = imagesContainingPoint(RA, Dec, Args)
    % Search for DB images containing the given sky point
    %     a 2-step algorithm: (i) search by the neighboring healpix pixels
    %     (ii) exact search of point in the image among the images selected at the first step
    % Input  : - RA (deg or sexagesimal string)
    %          - Dec (deg or signed sexagesimal string)
    %          * ...,key,val,... 
    %         'DB' - input DB object (otherwise a default connection will be attempted)
    %         'DBName' - name of the DB
    %         'DBUser' - either a user name, or a {Project, User} cell array,
    %                    in which case the password is taken from PasswordsManager.
    %         'DBPass' - user pwd. If empty (default), the password is taken
    %                    from PasswordsManager according to 'DBUser'.
    %         'Table'  - image table name. If it does not contain a DB name,
    %                    then 'DBName' is prepended.
    %         'SelectFields' - columns to be drawn from the table: id_visit, ra1-4 and dec1-4 are mandatory
    %         'HP_ColName'   - name of the Healpix column
    %         'PrimarySearchNside' - nside of the primary search HPix, must match HP_ColName
    %         'PrimarySearchRad' - [deg] this radius should include all the neighboring pixels of PrimarySearchNside
    %         'Verbosity' - if > 0, report the fraction of the primarily
    %                    selected images that do contain the point. Default is 0.
    % Output : - a table containing unique indexes of the images, exptime, and jd_start
    % Author : A.M. Krassilchtchikov (2025 May)
    % Example: T=db.search.imagesContainingPoint("10:23:00","+40:20:00");
    %          T=db.search.imagesContainingPoint(100,10,'DB',D); % when D is a DB connected before
    arguments
        RA                     = 83.63;
        Dec                    = 22.01;
        Args.DB                = [];
        Args.DBName            = 'last';
        Args.DBUser            = {'last_ro','last_user'};  % {Project, User} in PasswordsManager
        Args.DBPass            = [];
        Args.Table             = 'visit_images'; % 'visit_images';
        Args.SelectFields      = ["id_visit", "exptime", "jd_start", "ra1", "ra2", "ra3", "ra4", "dec1", "dec2", "dec3", "dec4"];
        Args.HP_ColName        = 'upix_low';
        Args.PrimarySearchNside= 2^8; % this should match the actual HP_ColName
        Args.PrimarySearchRad  = 1;   % [deg] this radius should include all the neighboring pixels of PrimarySearchNside
        Args.Server            = @VO.name.server_simbad;
        Args.Verbosity         = 0;
    end
    %
    Ncoo   = numel(RA);
    Result = cell(Ncoo,1);
    % get a connection
    if isempty(Args.DB)
        DB = db.Db;
        DB.User = Args.DBUser;
        if ~isempty(Args.DBPass)
            DB.Password = Args.DBPass;
        end
        DB.Conn;
        DB.useDB(Args.DBName);
    else
        DB = Args.DB;
    end
    % qualify the table name, so that the query does not rely on the current DB
    if contains(Args.Table,'.')
        TableName = Args.Table;
    else
        TableName = sprintf('%s.%s', Args.DBName, Args.Table);
    end
    % loop the points 
    for Icoo = 1:Ncoo      
        if isnumeric(RA) % RA, Dec are in deg.
            RA0 = RA(Icoo); Dec0 = Dec(Icoo);
        else             % RA, Dec are a cell array of sexadecimal strings            
            [RA0, Dec0]=celestial.convert.cooResolve(RA{Icoo}, Dec{Icoo}, 'InUnits','sex', 'OutUnits','deg', 'Server',Args.Server);
        end
        % first we search by the neighboring healpix pixels:
        WhereClause = db.search.queryConeSearch_Healpix(RA0, Dec0,...
            Args.PrimarySearchRad,'SearchRadiusUnits','deg','NSide',Args.PrimarySearchNside, ...
            'HP_ColName',Args.HP_ColName);
        Query = db.Db.genQuery(TableName, Args.SelectFields, WhereClause);
        T     = DB.query(Query);
        N1    = height(T);
        F     = false(N1,1);
        % next we select only those images that indeed contain the point:
        for Irow = 1:N1
            Pol = [T.ra1(Irow),T.dec1(Irow);T.ra2(Irow),T.dec2(Irow);T.ra3(Irow),T.dec3(Irow);T.ra4(Irow),T.dec4(Irow)];
            F(Irow) = celestial.search.isPointInsidePolygon(RA0, Dec0, Pol);
        end
        Result{Icoo} = sortrows(T(F,:),'jd_start'); % select and sort by start time
        if Args.Verbosity > 0 && N1 > 0
            fprintf('second selection: %.3f\n', height(Result{Icoo})./N1);
        end
    end
end

