function [Result] = imagesContainingPoint(RA, Dec, Args)
    % Search for DB images containing the given sky point
    %     A 2-step algorithm: (i) search by the neighboring healpix pixels
    %     (ii) exact search of point in the image among the images selected at the first step
    % Input  : - RA (deg or sexagesimal string)
    %          - Dec (deg or sexagesimal string)
    %          
    %          * ...,key,val,... 
    % Output : - a table containing unique indexes of the images
    % Author : A.M. Krassilchtchikov (2025 May) 
    % Example: 

    arguments
        RA                     = 83.63;
        Dec                    = 22.01;
        Args.DB                = [];
        Args.DBName            = 'last';
        Args.DBUser            = 'last_user';
        Args.DBPass            = 'physics';
        Args.Table             = 'vis_im_tst_dedup'; % 'visit_images';   
        Args.SelectFields      = ["id_visit", "ra1", "ra2", "ra3", "ra4", "dec1", "dec2", "dec3", "dec4", "exptime"];        
        Args.HP_ColName        = 'upix_low';  
        Args.PrimarySearchNside= 2^8; % this should match the actual HP_ColName         
        Args.PrimarySearchRad  = 1;   % [deg] this radius should include all the neighboring pixels of PrimarySearchNside
        
    end
    
    % get a connection
    if isempty(Args.DB)
        DB = db.Db;
        DB.User = Args.DBUser;
        DB.Password = Args.DBPass;        
        DB.Conn;
        DB.useDB = Args.DBName;  
    else
        DB = Args.DB;
    end
    
    Ncoo = numel(RA);
    Result  = cell(Ncoo,1);
    
    for Icoo = 1:Ncoo        
        % first we search by the neighboring healpix pixels:
        WhereClause = db.search.queryConeSearch_Healpix(RA, Dec,...
            Args.PrimarySearchRad,'SearchRadiusUnits','deg','NSide',Args.PrimarySearchNside, ...
            'HP_ColName',Args.HP_ColName);
        QuerySQL = db.Db.genQuery(Args.Table, Args.SelectFields, WhereClause);
        T        = DB.query(QuerySQL);
        N1       = height(T);
        F        = false(N1,1);
        
        % next we select only those images that indeed contain the point:
        for Irow = 1:N1
            Pol = [T.ra1(Irow),T.dec1(Irow);T.ra2(Irow),T.dec2(Irow);T.ra3(Irow),T.dec3(Irow);T.ra4(Irow),T.dec4(Irow)];
            F(Irow) = celestial.search.isPointInsidePolygon(RA(Icoo), Dec(Icoo), Pol);
        end
        Result{Icoo} = T(F,:);
        N2      = height(Result{Icoo}); 
        fprintf('second selection: %.2d\n',N2/N1); % diagnostic
    end
end
