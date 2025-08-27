function [T, DB] = searchVisitsByCoo(RA, Dec, Args)
    % Search LAST visits by coordinates
    %   see also: pipeline.last.queryDB.searchVisits
    % Input  : - Either:
    %            J2000 RA [deg|rad|sexagesimal string]
    %            or: Object name string (in this case Dec (2nd argument) must be
    %            empty.
    %            or: A cell array of {fieldid, camnum, cropid}
    %            where cropid can be a vector of several elements.
    %            or: a table which which will be returned as is.
    %          - J2000.0 Dec, or empty Default is [].
    %          * ...,key,val,... 
    %            'UseCorners' - Logical indicating if to select only
    %                   images that search coordinates fall within the 4 corners of the image.
    %                   Default is true.
    %            'HalfWidth' -The [RA, Dec] half width of the crop image
    %                   size. This is used in case images are searched by
    %                   coordinates. Default is [0.55 0.55]./1
    %            'MaxNim' - Maximum number of visits to search.
    %                   Default is 1e6.
    %            'SortBy' - if number of images found exceeds 'MaxNim',
    %                   then will sort them by this column and choose the
    %                   top MaxNim. Default is 'fwhm'.
    %            'InUnits' - Units of RA/Dec coordinates. Default is 'deg'.
    %            'Constraints' - A two column cell array of constraints to
    %                   apply to the image search (by coordinates or
    %                   fieldid).
    %                   Default is: {'fwhm',[1.0 4.0]; 'airmass',[1 1.5]; 'ph_rms',[0 0.03]; 'limmag',[20 23]};
    %            'DB' - A db.Db object. If empty, will be created and
    %                   opened, and returned. Default is [].
    %
    %            See code, for additional (hidden) arguments.
    %
    % Output : - A cell array of tables of the selected images used for the coaddition.
    %            Cell element per coordinate search.
    %          - The db.Db object.
    % Author : Eran Ofek (2024 Dec) 
    % Example: T=pipeline.last.queryDB.searchVisitsByCoo(120.8,10.5);
    %          T=pipeline.last.queryDB.searchVisitsByCoo('M31')
    %          T=pipeline.last.queryDB.searchVisitsByCoo('M31',[],'Constraints',{'jd_start',[2451545 2461000]; 'fwhm',[1 4]})
    %
    %          % Tests:
    %          RA = 40.5229121965; Dec = -16.9563601815;
    %          D=db.Db; D.useDB('last');
    %          TTT=D.query('SELECT * FROM visit_images');
    %          Flag = celestial.coo.findInBox(RA, Dec, [TTT.ra1, TTT.ra2, TTT.ra3, TTT.ra4], [TTT.dec1, TTT.dec2, TTT.dec3, TTT.dec4]);
    %          T=pipeline.last.queryDB.searchVisitsByCoo(RA,Dec);

    arguments
        RA                             % J2000 RA [deg|rad|sexagesimal|{FieldID#, CamNum, CropID}|table]
        Dec                    = [];

        Args.InitSearchRadius  = 2000;  % [arcsec]
        Args.NSide_Low         = 2.^8;
        Args.QueryMethod       = 'upix'; %'radec';
        Args.UseCorners        = true;

        Args.HalfWidth         = 2.*[0.55 0.55]./1;
        Args.MaxNim            = []; %1e7;  % maximum number of images to add
        Args.SortBy            = 'fwhm';

        Args.Constraints       = {'fwhm',[1.0 4.0]; 'airmass',[1 1.5]; 'ph_rms',[0 0.03]; 'limmag',[20 23]};
        Args.RangeJD           = [];
        Args.InUnits           = 'deg';
        Args.DB                = [];
        
        Args.Server            = @VO.name.server_simbad;
        
        Args.TableName         = "last.visit_images";

        Args.SelectFields      = ["id_visit", "ra", "dec", "m_ra", "m_dec", "airmass", "exptime", "jd_start", "midjd", "filter", "fieldid", "counter",...
                                  "nodenumb", "mountnum", "camnum", "ccdid", "cropid", "subdir", "server",...
                                  "cloud", "transper_z", "fwhm_dimm_z", "ast_nsrc", "ast_arms", "ast_errm",...
                                  "meanbck", "medbck", "stdbck", "meanvar", "medvar", "fwhm", "med_a", "med_b", "med_th", "nsrc",...
                                  "ph_zp", "ph_col1", "ph_medc", "ph_rms", "ph_nsrc", "limmag", "backmag", "ncoadd",...
                                  "ra1", "ra2", "ra3", "ra4", "dec1", "dec2", "dec3", "dec4", "optics_cln",...
                                  "upix_partition", "upix_low", "upix_high"];
        
        Args.ColCornerRA       = ["ra1","ra2","ra3","ra4"];
        Args.ColCornerDec      = ["dec1","dec2","dec3","dec4"];

    end
    RAD = 180./pi;

    % resove coordinates
    % Output is J2000.0 RA/Dec
    DB = Args.DB;
    Ncrop = 1;
    if istable(RA)
        % assume input is the output of query
        % will coadd all the images listed in table
        T{1} = RA;
        
    else
        % create table by query DB

        
        [RA, Dec] = celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','deg', 'Server',Args.Server);
        
    
        % make DB and connect
        if isempty(Args.DB)
            DB = db.Db;
            %DB
            DB.connect;
            DB.useDB('last');
        else
            DB = Args.DB;
        end

        if ~isempty(Args.RangeJD)
            Nc = size(Args.Constraints,1);
            Args.Constraints{Nc+1,1} = 'jd_start';
            Args.Constraints{Nc+1,2} = [Args.RangeJD];
        end
        
    
        
    
        Args.HalfWidth = convert.angular(Args.InUnits, 'deg', Args.HalfWidth);
        
        Ncoo = numel(RA);
        T    = cell(Ncoo,1);
        for Icoo=1:1:Ncoo

            % query by coordinates
            
            switch Args.QueryMethod
                case 'radec'
                    %tic;
                    PosConst    = db.search.queryCooBoxConstraints(RA(Icoo), Dec(Icoo), 'HalfWidth',Args.HalfWidth, 'ColRA','ra', 'ColDec','dec');
                    Constraints = [PosConst; Args.Constraints];
                    QuerySQL    = db.Db.genQuery(Args.TableName, Args.SelectFields, Constraints, 'SortBy',Args.SortBy, 'Top',Args.MaxNim);
                    %toc
                    %error('Search by coordinates not supported yet');
                    %tic;
                    T{Icoo} = DB.query(QuerySQL);
                    %toc
    
                case 'upix'
    
                    %tic;
                    [WhereClause,HP] = db.search.queryConeSearch_Healpix(RA(Icoo), Dec(Icoo), Args.InitSearchRadius,'NSide',Args.NSide_Low, 'HP_ColName','upix_low');
                    AddWhere    = db.Db.genWhereClause(Args.Constraints,'AddWhere',false);
                    WhereClause = sprintf('%s AND %s', WhereClause, AddWhere);
                    QuerySQL    = db.Db.genQuery(Args.TableName, Args.SelectFields, WhereClause);
                    %toc
            
                    %tic;
                    T{Icoo} = DB.query(QuerySQL);
                    %toc
    
                otherwise
                    error('Unknown QueryMethod option');
            end
          
            % refine selection by exact corners
            if Args.UseCorners
                Ncand = size(T{Icoo},1);
                Flag  = false(Ncand,1);
                for Icand=1:1:Ncand
                    Corners = [T{Icoo}.(Args.ColCornerRA{1})(Icand), T{Icoo}.(Args.ColCornerDec{1})(Icand);...
                               T{Icoo}.(Args.ColCornerRA{2})(Icand), T{Icoo}.(Args.ColCornerDec{2})(Icand);...
                               T{Icoo}.(Args.ColCornerRA{3})(Icand), T{Icoo}.(Args.ColCornerDec{3})(Icand);...
                               T{Icoo}.(Args.ColCornerRA{4})(Icand), T{Icoo}.(Args.ColCornerDec{4})(Icand)];
                    Flag(Icand) = celestial.htm.in_polysphere([RA(Icoo), Dec(Icoo)]./RAD, Corners./RAD);
                    %Flag1(Icand) = celestial.coo.findInBox(RA(Icoo), Dec(Icoo), Corners(:,1).', Corners(:,2).', 'InUnits','deg');

                end
                
                T{Icoo} = T{Icoo}(Flag,:);

            end

        end
    end

    if isempty(Args.DB) && nargout<2 && ~istable(RA)
        % disconnect DB
        DB.disconnect;
    end

end
