function [Result, DB] = coadd(RA, Dec, Args)
    % One line description
    %     Optional detailed description
    % Input  : - 
    %          - 
    %          * ...,key,val,... 
    % Output : - 
    % Author : Eran Ofek (2024 Dec) 
    % Example: 

    arguments
        RA    % J2000 RA [deg|rad|sexagesimal|[FieldID#, CamNum, CropID]|table]
        Dec
        Args.InUnits           = 'deg';
        Args.Server            = @VO.name.server_simbad;
        Args.DB                = [];
        Args.TableName         = "last.last_vistits"

        Args.SelectFields      = ["ra", "dec", "m_ra", "m_dec", "airmass", "exptime", "midjd", "filter", "fieldid", "counter", "nodenumb", "mountnum", "camnum", "ccdid", "cropid", "subdir", "server",...
                                  "cloud", "transper_z", "fwhm_dimm_z", "ast_nsrc", "ast_arms", "ast_errm",...
                                  "meanbck", "medbck", "stdbck", "meanvar", "medvar", "fwhm", "med_a", "med_b", "med_th", "nsrc",...
                                  "ph_zp", "ph_col1", "ph_medc", "ph_rms", "ph_nsrc", "limmag", "backmag", "ncoadd",...
                                  "ra1", "ra2", "ra3", "ra4", "dec1", "dec2", "dec3", "dec4", "optics_cln"];
        Args.Constraints       = {'fwhm',[1.0 4.0], 'airmass',[1 1.5], 'ph_rms',[0 0.03], 'limmag',[20 22]};
    end

    % resove coordinates
    % Output is J2000.0 RA/Dec
    if istable(RA)
        % assume input is the output of query
        % will coadd all the images listed in table
        T = RA;
    else
        % create table by query DB

        if isempty(Dec) && isnumeric(RA)
            % RA contains numeric fieldid
            FieldID = RA(1);
            CamNum  = RA(2);
            CropID  = RA(3);
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
            DB.connect;
        else
            DB = Args.DB;
        end
    
        if isempty(FieldID)
            % query by coordinates
    
            error('Search by coordinates not supported yet');
            
    
        else
            % query by FieldID
            AddConst = db.Db.genWhereClause({'fieldid',sprintf('%s%',FieldID); 'camnum',CamNum; 'cropid',CropID}, 'AddWhere',false);
            Constraints = [AddConst, Args.Constraints];
            QuerySQL = db.Db.genQuery('last_vistits', Args.SelectFields, Constraints);
            T = DB.query(QuerySQL);
    
        end
    end


    






end
