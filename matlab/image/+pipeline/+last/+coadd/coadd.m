function [CI, DB, AI, T] = coadd(RA, Dec, Args)
    % Coadd LAST images by coordinates or fieldid/camnum/cropid
    % Input  : - Either:
    %            J2000 RA [deg|rad|sexagesimal string]
    %            or: Object name string (in this case Dec (2nd argument) must be
    %            empty.
    %            or: A cell array of {fieldid, camnum, cropid}
    %            where cropid can be a vector of several elements.
    %            or: a table which is the output of a visit_images table
    %            (all the images in the table will be coadd with no
    %            post spelection).
    %          - J2000.0 Dec, or empty Default is [].
    %          * ...,key,val,... 
    %            'HalfWidth' -The [RA, Dec] half width pof the crop image
    %                   size. This is used in case images are searched by
    %                   coordinates. Default is [0.55 0.55]./2
    %            'MinNim' - Minimum number of images to coadd.
    %                   Default is 5.
    %            'MaxNim' - Maximum number of images to coadd.
    %                   Default is 100.
    %            'SortBy' - if number of images found exceeds 'MaxNim',
    %                   then will sort them by this column and choose the
    %                   top MaxNim. Default is 'fwhm'.
    %            'StackMethod' - Coadd method. Default is 'sigmaclip'.
    %            'StackArgs' - A cell array of additional arguments to pass
    %                   to the coadd function: imProc.stack.coaddW
    %                   Default is {'MeanFun',@tools.math.stat.nanmean, 'StdFun', @tools.math.stat.std_mad, 'Nsigma',[2 2]}
    %            'FindSrc' - A logical indicating if to populate the
    %                   CatData (source catalog) in the coadd image.
    %                   Default is true.
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
    % Output : - An AstroImage (array) of coadd images (image per cropid).
    %          - The db.Db object.
    %          - An AstroImage array of the coadd images. If multiple
    %            coaddition are performed, this contains the images for the
    %            last coaddition (i.e., CI(end)).
    %          - A cell array of tables of the selected images used for the coaddition.
    %            Cell element per coadd image.
    % Author : Eran Ofek (2024 Dec) 
    % Example: CI=pipeline.last.coadd.coadd({1325 2 20});
    %          CI=pipeline.last.coadd.coadd(100,10);

    arguments
        RA                             % J2000 RA [deg|rad|sexagesimal|{FieldID#, CamNum, CropID}|table]
        Dec                    = [];

        Args.HalfWidth         = [0.55 0.55]./2;
        Args.MinNim            = 5;    % minimum number of images to add
        Args.MaxNim            = 100;  % maximum number of images to add
        Args.SortBy            = 'fwhm';

        Args.StackMethod       = 'sigmaclip';      
        Args.StackArgs         = {'MeanFun',@tools.math.stat.nanmean, 'StdFun', @tools.math.stat.std_mad, 'Nsigma',[2 2]};

        Args.FindSrc logical   = true;
        Args.Constraints       = {'fwhm',[1.0 4.0]; 'airmass',[1 1.5]; 'ph_rms',[0 0.03]; 'limmag',[20 23]};

        Args.InUnits           = 'deg';
        Args.DB                = [];
        
        Args.Server            = @VO.name.server_simbad;
        
        Args.TableName         = "last.visit_images"

        Args.SelectFields      = ["ra", "dec", "m_ra", "m_dec", "airmass", "exptime", "jd_start", "midjd", "filter", "fieldid", "counter", "nodenumb", "mountnum", "camnum", "ccdid", "cropid", "subdir", "server",...
                                  "cloud", "transper_z", "fwhm_dimm_z", "ast_nsrc", "ast_arms", "ast_errm",...
                                  "meanbck", "medbck", "stdbck", "meanvar", "medvar", "fwhm", "med_a", "med_b", "med_th", "nsrc",...
                                  "ph_zp", "ph_col1", "ph_medc", "ph_rms", "ph_nsrc", "limmag", "backmag", "ncoadd",...
                                  "ra1", "ra2", "ra3", "ra4", "dec1", "dec2", "dec3", "dec4", "optics_cln"];

        Args.CoaddLevel        = 'coadd';
        Args.CoaddProduct      = 'Image+';
    end

    % resove coordinates
    % Output is J2000.0 RA/Dec
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
            
            PosConst    = db.Db.genCooBoxConstraints(RA, Dec, 'HalfWidth',Args.HalfWidth);
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

    
    
    

    for Icrop=1:1:Ncrop

        Nim = size(T{Icrop},1);
        if Nim<Args.MinNim
            warning('Not enough images (%d)',Nim);
            CI(Icrop) = AstroImage;
        else
            [AI, AllPaths, AllFiles] = pipeline.last.queryDB.loadProducts(T{Icrop}, Args.CoaddLevel, Args.CoaddProduct);
            
            Nimages = numel(AI);
            
            % need background and variance due to a bug in coadd:
            AI = imProc.background.background(AI, 'SubSizeXY',[]);
            [~,Back,Var]=imProc.stat.mean(AI);
            
            % register and coadd
            AI = imProc.transIm.interp2wcs(AI, AI(1));
            %AI(:,Icrop) = imProc.transIm.interp2wcs(AI(:,Icrop),AI(1,Icrop), 'DataProp',{'Image','Back','Var','Mask'});
    
            NB = numel(AI);
            for IB=1:1:NB
                AI(IB).Back = Back(IB);
                AI(IB).Var  = Var(IB);
            end
            
            %CI(Icrop)   = imProc.stack.coadd(AI(:,Icrop), 'StackMethod',Args.StackMethod, 'StackArgs',Args.StackArgs, 'UseWeights',false);
            CI(Icrop)   = imProc.stack.coaddW(AI, 'StackMethod',Args.StackMethod, 'StackArgs',Args.StackArgs);
        
            CI(Icrop).UserData.Nimages = Nimages;
        
           % Args.FindSrc=false;
            if Args.FindSrc
                %
                %CI=imProc.background.background(CI, 'BackFun',@median,'BackFunPar',{'omitnan'},'VarFun',@var,'VarFunPar',{'omitnan'}); 
                CI(Icrop).Back = imProc.stat.median(CI(Icrop)).*ones(size(CI(Icrop).Image));
                CI(Icrop).Var = imProc.stat.rstd(CI(Icrop)).^2 .*ones(size(CI(Icrop).Image));
                CI(Icrop) = imProc.sources.findMeasureSources(CI(Icrop),'AddFlags',false);
                CI(Icrop) = imProc.astrometry.addCoordinates2catalog(CI(Icrop),'OutUnits','deg');
                CI(Icrop) = imProc.psf.populatePSF(CI(Icrop));
                CI(Icrop) = imProc.calib.photometricZP(CI(Icrop));
                
            end
        end
    end
    
    

end
