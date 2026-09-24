function [T, DB] = searchVisits(RA, Dec, Args)
    % Search LAST visits by coordinates or fieldid/camnum/cropid
    %   Use: pipeline.last.queryDB.searchVisitsByCoo for exact coo. search
    % Input  : - Either:
    %            J2000 RA [deg|rad|sexagesimal string]
    %            or: Object name string (in this case Dec (2nd argument) must be
    %            empty.
    %            or: A cell array of {fieldid, camnum, cropid}
    %            where cropid can be a vector of several elements.
    %            or: a table which which will be returned as is.
    %          - J2000.0 Dec, or empty Default is [].
    %          * ...,key,val,... 
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
    %            Cell element per coadd image.
    %          - The db.Db object.
    % Author : Eran Ofek (2024 Dec) 
    % Example: T=pipeline.last.queryDB.searchVisits({1325 2 20});
    %          T=pipeline.last.queryDB.searchVisits(100,10);
    %          T=pipeline.last.queryDB.searchVisits('M31')
    %          T=pipeline.last.queryDB.searchVisits('M31',[],'Constraints',{'jd_start',[2451545 2461000]; 'fwhm',[1 4]})

    arguments
        RA                             % J2000 RA [deg|rad|sexagesimal|{FieldID#, CamNum, CropID}|table]
        Dec                    = [];

        Args.HalfWidth         = [0.55 0.55]./1;
        Args.MaxNim            = 1e6;  % maximum number of images to add
        Args.SortBy            = 'fwhm';

        Args.Constraints       = {'fwhm',[1.0 4.0]; 'airmass',[1 1.5]; 'ph_rms',[0 0.03]; 'limmag',[20 23]};

        Args.InUnits           = 'deg';
        Args.DB                = [];
        
        Args.Server            = @VO.name.server_simbad;
        
        Args.TableName         = "last.visit_images";

        Args.SelectFields      = ["ra", "dec", "m_ra", "m_dec", "airmass", "exptime", "jd_start", "midjd", "filter", "fieldid", "counter", "nodenumb", "mountnum", "camnum", "ccdid", "cropid", "subdir", "server",...
                                  "cloud", "transper_z", "fwhm_dimm_z", "ast_nsrc", "ast_arms", "ast_errm",...
                                  "meanbck", "medbck", "stdbck", "meanvar", "medvar", "fwhm", "med_a", "med_b", "med_th", "nsrc",...
                                  "ph_zp", "ph_col1", "ph_medc", "ph_rms", "ph_nsrc", "limmag", "backmag", "ncoadd",...
                                  "ra1", "ra2", "ra3", "ra4", "dec1", "dec2", "dec3", "dec4", "optics_cln"];

    end

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

        if isempty(Dec) && iscell(RA)
            % RA contains numeric fieldid
            FieldID = RA{1};
            CamNum  = RA{2};
            CropID  = RA{3};
            Ncrop   = numel(CropID);
            RA      = [];
            Dec     = [];
        else
            FieldID = [];
            CamNum  = [];
            CropID  = [];
            [RA, Dec, FieldID] = celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','deg', 'Server',Args.Server);
        end
    
        % make DB and connect
        if isempty(Args.DB)
            DB = db.Db;
            %DB
            DB.connect;
            DB.useDB('last');
        else
            DB = Args.DB;
        end
    
        if isempty(FieldID)
            % query by coordinates
    
            Args.HalfWidth = convert.angular(Args.InUnits, 'deg', Args.HalfWidth);
            
            PosConst    = db.search.queryCooBoxConstraints(RA, Dec, 'HalfWidth',Args.HalfWidth);
            Constraints = [PosConst; Args.Constraints];
            QuerySQL    = db.Db.genQuery(Args.TableName, Args.SelectFields, Constraints, 'SortBy',Args.SortBy, 'Top',Args.MaxNim);
            
            %error('Search by coordinates not supported yet');
            T{1} = DB.query(QuerySQL);
    
        else
            % query by FieldID
            for Icrop=1:1:Ncrop
                if ischar(Args.Constraints) || isstring(Args.Constraints)
                    AddConst    = db.Db.genWhereClause({'fieldid',sprintf('%d%%',FieldID); 'camnum',CamNum; 'cropid',CropID(Icrop)}, 'AddWhere',false);
                    Constraints = [AddConst, 'AND', Args.Constraints];
                else
                    Constraints = [{'fieldid',sprintf('%d%%',FieldID); 'camnum',CamNum; 'cropid',CropID(Icrop)}; Args.Constraints];
                end
                QuerySQL = db.Db.genQuery(Args.TableName, Args.SelectFields, Constraints, 'SortBy',Args.SortBy, 'Top',Args.MaxNim);
                T{Icrop} = DB.query(QuerySQL);
            end
        end
    end

    if isempty(Args.DB) && nargout<2 && ~istable(RA)
        % disconnect DB
        DB.disconnect;
    end

end
