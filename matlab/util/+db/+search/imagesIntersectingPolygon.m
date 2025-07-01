function [Result] = imagesIntersectingPolygon(P, Args)
    % Search for DB images intersecing with or containing the given sky polygon
    %     Designed mostly for use with 'reference_images' tables 
    % Input  : - polygon: Nx2 array of [RA, Dec] in degrees 
    %          - 
    %          * ...,key,val,... 
    %         'DB' - input DB object (otherwise a default connection will be attempted)
    %         'DBName' - name of the DB
    %         'DBUser' - read-only user name
    %         'DBPass' - read-only user pwd
    %         'Table'  - image table name
    %         'SelectFields' - columns to be drawn from the table: id_visit, ra1-4 and dec1-4 are mandatory
    %         'HP_ColName'   - name of the Healpix column
    %         'Resolution'   - [arcsec] desired accuracy = raster resolution 
    %         'MaxImageSize' - [deg] maximal size of the DB image
    % Output : - a table containing unique indexes of the images, exptime,
    %            jd_start, and a column indicating intersection and containment 
    % Author : A.M. Krassilchtchikov (2025 Jun)
    % Example: P0 = [10, 70; 10, 70.5; 9.5, 70.5; 9.5, 70];
    %          T0 = db.search.imagesIntersectingPolygon(P0); % just intersection
    %          P1 = [10, 70; 10, 70.2; 9.8, 70.2; 9.8, 70];
    %          T1 = db.search.imagesIntersectingPolygon(P1); % some of the images contain the polygon   
    %          P2 = [9.5, 19.5; 9.5, 20.5; 8.5, 20.5; 8.5, 19.5];
    %          T2 = db.search.imagesIntersectingPolygon(P2,'Resolution',10); % the polygon contains some of the images 
    arguments
        P
        Args.DB                = [];
        Args.DBName            = 'last';
        Args.DBUser            = 'last_user';
%         Args.AstroDBPassFile   = '~/.astropack/Passwords.yml'; 
        Args.DBPass            = 'physics';
        Args.Table             = 'vis_im_tst_dedup'; % will be mostly used for 'reference_images'    
        Args.SelectFields      = ["id_visit", "exptime", "jd_start", "ra1", "ra2", "ra3", "ra4", "dec1", "dec2", "dec3", "dec4"];        
        Args.PrimarySearchNside= 2^8; % this should match the actual HP_ColName
        Args.HP_ColName        = 'upix_low'; 
        Args.Resolution        = 10;  % [arcsec] raster resolution
        Args.MaxImageSize      = 0.7; % [deg] maximal size of the DB image
    end
    % get a connection
    if isempty(Args.DB)
        DB = db.Db;
        DB.User = Args.DBUser;
%         Configuration.getSingleton().loadFile(Args.AstroDBPassFile); % tell the PM where to look for passwords
%         PM = PasswordsManager; DB.Password = PM.search(Args.DBName).Pass;
        DB.Password = Args.DBPass;        
        DB.Conn;
        DB.useDB(Args.DBName);  
    else
        DB = Args.DB;
    end
    % determine the center and the size of the polygon: 
    [RA0, Dec0, R0] = celestial.polygon.spherical_polygon_circum_circle(P);     
    % first crudely select images within Rad from the polygon's center:
    Rad = R0 + Args.MaxImageSize;
    WhereClause = db.search.queryConeSearch_Healpix(RA0, Dec0, Rad,...
            'SearchRadiusUnits','deg','NSide',Args.PrimarySearchNside, 'HP_ColName',Args.HP_ColName);
    Query = db.Db.genQuery(Args.Table, Args.SelectFields, WhereClause);
    T     = DB.query(Query);
    N1    = height(T); Empty = zeros(N1,1);
    T     = [T, table(Empty, Empty, Empty, ...
        'VariableNames', {'Intersect', 'P0containP1','P1containP0'})];
    % check the intersections:
    R = celestial.healpix.rasterize_polygon(P,'Resolution',Args.Resolution); % raster the polygon once to save time in polygon_boolean_operations
    for Irow = 1:N1
       Image = [T.ra1(Irow),T.dec1(Irow);T.ra2(Irow),T.dec2(Irow);T.ra3(Irow),T.dec3(Irow);T.ra4(Irow),T.dec4(Irow)];
       Res   = celestial.coo.polygon_boolean_operations(P, Image,'R0',R,'Resolution',Args.Resolution);       
       T.Intersect(Irow)   = Res.Intersect; % in fact, this column is needed for tests only 
       T.P0containP1(Irow) = Res.P0containP1; T.P1containP0(Irow) = Res.P1containP0;
    end
    Result = sortrows(T(T.Intersect>0,:),'jd_start'); % select and sort by start time
    N2     = height(Result);
    fprintf('second selection: %.3f\n',N2/N1);  % diagnostic
end
