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
        RA    % J2000 RA [deg|rad|sexagesimal|FieldID#|table]
        Dec
        Args.InUnits           = 'deg';
        Args.Server            = @VO.name.server_simbad;
        Args.DB                = [];
        Args.TableName         = "last.last_vistits"

        Args.QueryFields       = ["ra", "dec", "m_ra", "m_dec", "airmass", "exptime", "midjd", "filter", "fieldid", "counter", "nodenumb", "mountnum", "camnum", "ccdid", "cropid", "subdir", "server",...
                                  "cloud", "transper_z", "fwhm_dimm_z", "ast_nsrc", "ast_arms", "ast_errm",...
                                  "meanbck", "medbck", "stdbck", "meanvar", "medvar", "fwhm", "med_a", "med_b", "med_th", "nsrc", "ph_zp", "ph_col1", "ph_medc", "ph_rms", "ph_nsrc", "limmag", "backmag", "ncoadd",...
                                  "ra1", "ra2", "ra3", "ra4", "dec1", "dec2", "dec3", "dec4", "optics_cln"];
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
            FieldID = RA;
            RA      = [];
            Dec     = [];
        else
            FieldID = [];
            [RA, Dec, ObjectName] = celestial.convert.cooResolve(RA, Dec, 'InUnits',Args.InUnits, 'OutUnits','deg', 'Server',Args.Server);
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
    
    
        else
            % query by FieldID
            db.Db.genQuery('last_vistits', {'ra','dec'}, 'mag_psf<15')
            T = DB.query('SELECT ')
    
    
        end
    end


    






end
