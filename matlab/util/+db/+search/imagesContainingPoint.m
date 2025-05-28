function [Result] = imagesContainingPoint(RA, Dec, Args)
    % Search for DB images containing the given sky point
    %     a 2-step algorithm: (i) search by the neighboring healpix pixels
    %     (ii) exact search of point in the image among the images selected at the first step
    % Input  : - RA (deg or sexagesimal string)
    %          - Dec (deg or signed sexagesimal string)
    %          * ...,key,val,... 
    % Output : - a table containing unique indexes of the images, exptime, and jd_start 
    % Author : A.M. Krassilchtchikov (2025 May) 
    % Example: T=db.search.imagesContainingPoint("10:23:00","+40:20:00");
    %          T=db.search.imagesContainingPoint(100,10,'DB',D); % when D is a DB connected before  
    arguments
        RA                     = 83.63;
        Dec                    = 22.01;
        Args.DB                = [];
        Args.DBName            = 'last';
        Args.DBUser            = 'last_user';
        Args.DBPass            = 'physics';
        Args.Table             = 'vis_im_tst_dedup'; % 'visit_images';   
        Args.SelectFields      = ["id_visit", "exptime", "jd_start", "ra1", "ra2", "ra3", "ra4", "dec1", "dec2", "dec3", "dec4"];        
        Args.HP_ColName        = 'upix_low';  
        Args.PrimarySearchNside= 2^8; % this should match the actual HP_ColName         
        Args.PrimarySearchRad  = 1;   % [deg] this radius should include all the neighboring pixels of PrimarySearchNside
        Args.Server            = @VO.name.server_simbad; 
    end   
    %
    Ncoo   = numel(RA);
    Result = cell(Ncoo,1);
    % get a connection
    if isempty(Args.DB)
        DB = db.Db;
        DB.User = Args.DBUser;
        DB.Password = Args.DBPass;        
        DB.Conn;
        DB.useDB(Args.DBName);  
    else
        DB = Args.DB;
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
        Query = db.Db.genQuery(Args.Table, Args.SelectFields, WhereClause);
        T     = DB.query(Query);
        N1    = height(T);
        F     = false(N1,1);        
        % next we select only those images that indeed contain the point:
        for Irow = 1:N1
            Pol = [T.ra1(Irow),T.dec1(Irow);T.ra2(Irow),T.dec2(Irow);T.ra3(Irow),T.dec3(Irow);T.ra4(Irow),T.dec4(Irow)];
            F(Irow) = celestial.search.isPointInsidePolygon(RA0, Dec0, Pol);
        end
        Result{Icoo} = sortrows(T(F,:),'jd_start'); % select and sort by start time 
        N2      = height(Result{Icoo}); 
        fprintf('second selection: %.3f\n',N2/N1);  % diagnostic             
    end
end
