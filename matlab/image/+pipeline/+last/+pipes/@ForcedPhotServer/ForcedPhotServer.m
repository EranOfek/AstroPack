% ForcedPhotServer - Forced photometry service for LAST images
%
% Example: FFS=pipeline.last.pipes.ForcedPhotServer.demon('DB',DB);
%

classdef ForcedPhotServer < Component
    % 
            
    properties       
        %
        DB           = [];
        
        DbName       = 'last';

        TableRequest = 'forcedphot_requests';   % must be of type: ReplacingMergeTree
        TableOutput  = 'forcedphotsub_output';

        CutoutPath   = '/lastdata/forcedphotsub'
        ObsCoo       = [35 30 415];  % [deg deg m]

    end
    
    properties (Constant, Hidden)
        User         = 'euclid/root';
    end

    methods % Constructor
       
        function Obj = ForcedPhotServer(DB)
            % Constructor for ForcedFphotServer
        
            arguments
                DB = [];
            end
            
            if isempty(DB)
                Obj.DB = db.Db;
                Obj.DB.User = Obj.User;
                Obj.DB.connect;
                Obj.DB.useDB(Obj.DbName);
            else
                Obj.DB = DB;
            end
            Obj.Logger.LogF.FileName = '~/ForcedPhotServer.log';
        end
        
    end
    
    methods % setter/getters
       
    end
      
    methods
        function T=searchTarget(Obj, RA, Dec, Args)
            % searchTarget by coo or fieldid
            % Input  : - self.
            %          - J2000.0 RA [deg]
            %          - J2000.0 Dec [deg]
            %          * ...,key,val,...
            %            'FieldID' - Field ID. Default is [].
            %                   If empty then use: pipeline.last.queryDB.searchVisitsByCoo
            %            'MountNum' - Default is [].
            %            'CamNum' - Default is [].
            %            'CropID' - Default is [].
            % Output : - A table of visits containing relevant data.
            % Author : Eran Ofek (Jun 2025)

            arguments
                Obj
                RA             
                Dec            
                Args.FieldID   = [];
                Args.MountNum  = [];
                Args.CamNum    = [];
                Args.CropID    = [];
                Args.StartJD   = 0;
                Args.EndJD     = 3e9;
    
                Args.searchVisitsByCooArgs = {};
            end

            if isempty(Args.FieldID)
                IsEmptyFieldID = true;
            else
                if numel(Args.FieldID{1})==0
                    IsEmptyFieldID = true;
                else
                    IsEmptyFieldID = false;
                end
            end
            if ~IsEmptyFieldID
                if isempty(Args.MountNum)
                    T = Obj.DB.query(sprintf("SELECT * FROM visit_images WHERE fieldid LIKE '%s' AND camnum=%d AND cropid=%d AND jd_start>%14.5f AND jd_start<%14.5f",Args.FieldID, Args.CamNum, Args.CropID, Args.StartJD, Args.EndJD));
                else
                    T = Obj.DB.query(sprintf("SELECT * FROM visit_images WHERE fieldid LIKE '%s' AND mountnum=%d AND camnum=%d AND cropid=%d AND jd_start>%14.5f AND jd_start<%14.5f",Args.FieldID, Args.MountNum, Args.CamNum, Args.CropID, Args.StartJD, Args.EndJD));
                end
            else
                T = pipeline.last.queryDB.searchVisitsByCoo(RA, Dec, 'DB', Obj.DB, Args.searchVisitsByCooArgs{:}, 'Constraints',{'jd_start',[Args.StartJD, Args.EndJD]});
                T = T{1};
            end
     
        end

        function createRequestsTable(Obj)
            % Re-create forced photometry requests table
            % Input  : - self.
            % Output : null
            % Author : Eran Ofek (Jul 2025)
            % Example: FFS.createRequestsTable


            % search for new request
            % TableRequest contains columns:
            %   request_id, user_id, ra, dec, subtraction (default is true), status (created with default=0), nphot (number of data points added), jd_start, jd_end, fieldid,
            %   nodenum, mountnum, camnum, cropid, useexistingref (default true), resub (default false),
            %   loadnew (default false), maxiter (default is 0),
            %   get_cutout (default is 0), insertion_time (default is
            %   now)

            % To create this table:
            [~,Error] = Obj.DB.query(sprintf('DROP TABLE IF EXISTS %s', Obj.TableRequest), 'IsExec',true)
            VarNames    = {'request_id', 'user_id', 'ra',     'dec',    'subtraction', 'status', 'checkexisting', 'nphot', 'jd_start', 'jd_end',  'n_epoch_max', 'fieldid', 'nodenumb', 'mountnum', 'camnum', 'cropid', 'ccdid', 'useexistingref', 'resub', 'loadnew', 'maxiter', 'get_cutout', 'insertion_time'};
            VarUnits    = ["UInt64",     "UInt16",  "Float64","Float64","UInt8",       "UInt8",  "UInt8",         "UInt32","Float64",  "Float64", "UInt16",      "String",  "UInt8",    "UInt8",    "UInt8",  "UInt8",  "UInt8", "UInt8",          "UInt8", "UInt8",   "UInt8",   "UInt8",      "DateTime64(3,'UTC')"];
            VarDefaults = {[],           0,         [],        [],      1,             0,        1,               [],      [],          [],       10,            [],        1,          [],         [],       [],       1,        1,               0,       0,         0,         0,            'now64(3)'};
            Obj.DB.createTable(Obj.TableRequest,VarNames, VarUnits, VarDefaults, 'Index', {'INDEX ra_dec_index (ra, dec) TYPE minmax GRANULARITY 64', 'INDEX request_id_index request_id TYPE minmax GRANULARITY 32', 'INDEX user_id_index user_id TYPE minmax GRANULARITY 1'},'OrderBy','insertion_time','Engine','ReplacingMergeTree()');
            
            % Insert example: 
            % Obj.DB.insertCharDump('forcedphot_requests',table(2,0,262.72824, 66.68995, 2460000,2470000,"1718",1,1,3,14, 1,'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid', 'nodenumb', 'mountnum', 'camnum', 'cropid', 'loadnew'}))
            % Obj.DB.insertCharDump('forcedphot_requests',table(3,0,260.5709627, 58.8638455, 2450000,2470000,"1632",1,3,1,10, 1,'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid', 'nodenumb', 'mountnum', 'camnum', 'cropid', 'loadnew'}))
            % 
            % INSERT INTO forcedphot_requests (request_id, user_id, ra, dec, jd_start, jd_end, fieldid, nodenumb, mountnum, camnum, cropid, loadnew) VALUES  ( 2, 0, 262.728240000000028, 66.6899499999999961, 2460000, 2470000, '1718', 1, 1, 3, 14, 1 )
            % user_id: 0 - tests, 1 - last pipe, 2 - cast, 3 - webaccess


        end
        
        function Par=addRequest(Obj, RA, Dec, Args)
            % Add forced photometry request to requests table
            % Input  : - J2000.0 RA [deg]
            %          - J2000.0 Dec [deg]
            %          * ...,key,val,...
            %            See code for options (columns in the table)
            % Output : - A structure with populated arguments (e.g., request_id)
            % Author : Eran Ofek (Jul 2025)
            % Example: FFS.addRequest(1,1)

            arguments
                Obj
                RA
                Dec
                Args.request_id  = [];
                Args.user_id     = 1;
                Args.jd_start    = [];
                Args.jd_end      = [];
                Args.fieldid     = [];
                Args.nodenumb    = [];
                Args.mountnum    = [];
                Args.camnum      = [];
                Args.cropid      = [];
                Args.loadnew     = 1;
                Args.n_epoch_max = 100;
            end

            if ~isempty(Args.fieldid)
                Args.fielid = string(Args.fieldid);
            end


            if isempty(Args.request_id)
                % generate request id
                JD = celestial.time.julday();
                JD0 = celestial.time.julday([1 1 2020]);

                Args.request_id = uint32(floor((JD - JD0).*86400.*1000));
            end

            Par = Args;
            Par.ra   = RA;
            Par.dec  = Dec;
            ArgNames = fieldnames(Par);
            Narg     = numel(ArgNames);

            K = 0;
            for Iarg=1:1:Narg
                if ~isempty(Par.(ArgNames{Iarg}))
                    K  = K + 1;
                    Val{K} = Par.(ArgNames{Iarg});
                    Key{K} = ArgNames{Iarg};
                end
            end
            Treq = table(Val{:}, 'VariableNames',Key);

            Obj.DB.insertCharDump(Obj.TableRequest, Treq);
            
            % Obj.DB.insertCharDump('forcedphot_requests',table(11, 0, 260.86351, 48.59987, 2460673.879583907, 2460873.879583907, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end'}))
            % Obj.DB.insertCharDump('forcedphot_requests',table(12, 0, 260.86351, 48.59987, 2460673.879583907, 2460873.879583907, "1578", 1, 1, 2, 14, 1, 100, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid','nodenumb', 'mountnum', 'camnum', 'cropid','loadnew','n_epoch_max'}))
            

        end
    end

    methods (Static) % demon
        function Obj=demon(Args)
            % Forced photometry service demon
            %   The demon is running in the background and waiting for
            %   requests in the DB (TableRequest). When a new request is found it is
            %   executed and the output is written to the DB TableOutput).
            % Input  : * ...,key,val,...  
            %            'DB' - DB object. If empty, then will be created.
            %                   Default is [].
            %            See more options in code.
            % Example: FPS=pipeline.last.pipes.ForcedPhotServer.demon('DB',DB)

            arguments
                Args.DB                = [];
                Args.PauseTime         = 1;
                Args.SummaryPrintout   = 3600;

                Args.UseExistingRef    = false;
                Args.ReSub             = false;
                Args.LoadNew           = false;
                Args.MaxIter           = 0;

                Args.insertHealPixArgs = {'ColRA','request_ra','ColDec','request_dec'};
            end
            STATUS_WAITING  = 0;
            STATUS_READY    = 1;
            STATUS_FAILED   = 10;
            STATUS_NOOBS    = 2;
            HostName       = tools.os.get_computer;

            Obj = pipeline.last.pipes.ForcedPhotServer(Args.DB);

            LoopInd = 0;
            RequestCounter = 0;
            TotNphot   = 0;
            TotNreq    = 0;
            while true
                pause(Args.PauseTime);
                LoopInd = LoopInd + 1;
                if mod(floor(LoopInd.*Args.PauseTime), floor(Args.SummaryPrintout))==0
                    [LoopInd, RequestCounter]
                    Msg = sprintf('Summary - Total requests: %d, phot points: %d',TotNreq, TotNphot);                    
                    Obj.writeLogMessage(Msg, 'Info', HostName);
                end

                % search for new request
                % TableRequest contains columns:
                %   request_id, user_id, ra, dec, subtraction (default is true), status (created with default=0), nphot (number of data points added), jd_start, jd_end, fieldid,
                %   nodenum, mountnum, camnum, cropid, useexistingref (default true), resub (default false),
                %   loadnew (default false), maxiter (default is 0),
                %   get_cutout (default is 0), insertion_time (default is
                %   now)

                % To create this table:
                %       VarNames    = {'request_id', 'user_id', 'ra',     'dec',    'subtraction', 'status', 'checkexisting', 'nphot', 'jd_start', 'jd_end',  'n_epoch_max', 'fieldid', 'nodenumb', 'mountnum', 'camnum', 'cropid', 'ccdid', 'useexistingref', 'resub', 'loadnew', 'maxiter', 'get_cutout', 'insertion_time'};
                %       VarUnits    = ["UInt64",     "UInt16",  "Float64","Float64","UInt8",       "UInt8",  "UInt8",         "UInt32","Float64",  "Float64", "UInt16",      "String",  "UInt8",    "UInt8",    "UInt8",  "UInt8",  "UInt8", "UInt8",          "UInt8", "UInt8",   "UInt8",   "UInt8",      "DateTime64(3,'UTC')"];
                %       VarDefaults = {[],           0,         [],        [],      1,             0,        1,               [],      [],          [],       10,            [],        1,          [],         [],       [],       1,        1,               0,       0,         0,         0,            'now64(3)'};
                %       Obj.DB.createTable('forcedphot_requests',VarNames, VarUnits, VarDefaults, 'Index', {'INDEX ra_dec_index (ra, dec) TYPE minmax GRANULARITY 64', 'INDEX request_id_index request_id TYPE minmax GRANULARITY 32', 'INDEX user_id_index user_id TYPE minmax GRANULARITY 1'},'OrderBy','insertion_time','Engine','ReplacingMergeTree()');
                %       [~,Error] = Obj.DB.query('DROP TABLE IF EXISTS forcedphot_requests', 'IsExec',true)
                % Insert example: 
                % Obj.DB.insertCharDump('forcedphot_requests',table(2,0,262.72824, 66.68995, 2460000,2470000,"1718",1,1,3,14, 1, 300, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid', 'nodenumb', 'mountnum', 'camnum', 'cropid', 'loadnew','n_epoch_max'}))
                %
                % Obj.DB.insertCharDump('forcedphot_requests',table(3,0,260.5709627, 58.8638455, 2450000,2470000,"1632",1,3,1,10, 1,'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid', 'nodenumb', 'mountnum', 'camnum', 'cropid', 'loadnew'}))
                %
                % Obj.DB.insertCharDump('forcedphot_requests',table(31,0,6.93442, 28.73525, 2450000,2470000,"1241",1,2,1,4, 1, 1000,10,'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid','nodenumb','mountnum', 'camnum', 'cropid','loadnew','n_epoch_max','maxiter'})) % 2025vll
                % Obj.DB.insertCharDump('forcedphot_requests',table(32,0,248.20490, 59.25189, 2450000,2470000,"1630",1,7,1,17, 1, 1000,10,'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid','nodenumb', 'mountnum', 'camnum', 'cropid','loadnew','n_epoch_max','maxiter'})) % 2025thj
                % Obj.DB.insertCharDump('forcedphot_requests',table(33,0,260.57096, 58.86383, 2450000,2470000,"1632",1,3,1,10, 1, 1000,10,'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid','nodenumb', 'mountnum', 'camnum', 'cropid','loadnew','n_epoch_max','maxiter'})) % 2025qfh
                %
                % Obj.DB.insertCharDump('forcedphot_requests',table(5,0,261.31232, 60.56540, 2450000,2470000,"1679",1,2,2,8, 1, 1000, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid','nodenumb', 'mountnum', 'camnum', 'cropid','loadnew','n_epoch_max'})) % 2025oiw
                % Obj.DB.insertCharDump('forcedphot_requests',table(6,0,262.72824, 66.68995, 2450000,2470000,"1718",1,1,3,14, 1, 1000, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid','nodenumb', 'mountnum', 'camnum', 'cropid','loadnew','n_epoch_max'})) % 2025mkd
                % Obj.DB.insertCharDump('forcedphot_requests',table(7,0, 278.59838, 52.98174, 2450000,2470000,"1581",1,3,1,10, 1, 1000, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid','nodenumb','mountnum', 'camnum', 'cropid','loadnew','n_epoch_max'})) %2025sqo
                % Obj.DB.insertCharDump('forcedphot_requests',table(31,0, 359.77891, 27.99430, 2450000,2470000,"1320",1,1,1,21, 1, 1000, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid','nodenumb','mountnum', 'camnum', 'cropid','loadnew','n_epoch_max'})) %2025sqo
                %
                % Obj.DB.insertCharDump('forcedphot_requests',table(8,0, 164.55324416617, 28.176660156811906, 2460685.5480378103, 2460725.5480378103, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end'})) % test dates
                % Obj.DB.insertCharDump('forcedphot_requests',table(9,0, 239.916918804, 27.1113477238, 2460705.9886484947, 2460905.9886484947, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end'})) % test dates
                % Obj.DB.insertCharDump('forcedphot_requests',table(21,0, 239.916918804, 27.1113477238, 2460705.9886484947, 2460905.9886484947, 1000, 0, 1, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','n_epoch_max','useexistingref','resub'})) % test dates
                % 
                % Obj.DB.insertCharDump('forcedphot_requests',table(11, 0, 260.86351, 48.59987, 2460673.879583907, 2460873.879583907, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end'}))
                % Obj.DB.insertCharDump('forcedphot_requests',table(12, 0, 260.86351, 48.59987, 2460673.879583907, 2460873.879583907, "1578", 1, 1, 2, 14, 1, 100, 'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid','nodenumb', 'mountnum', 'camnum', 'cropid','loadnew','n_epoch_max'}))
                %
                % 
                %
                % INSERT INTO forcedphot_requests (request_id, user_id, ra, dec, jd_start, jd_end, fieldid, nodenumb, mountnum, camnum, cropid, loadnew) VALUES  ( 2, 0, 262.728240000000028, 66.6899499999999961, 2460000, 2470000, '1718', 1, 1, 3, 14, 1 )
                % user_id: 0 - tests, 1 - last pipe, 2 - cast, 3 - webaccess

                % Tall = Obj.DB.query(sprintf("SELECT * FROM %s", Obj.TableRequest));
                Treq = Obj.DB.query(sprintf("SELECT * FROM %s WHERE status=%d AND request_id<1e15", Obj.TableRequest, STATUS_WAITING));

                if ~isempty(Treq)
                    
                    
                    % search target in visits
                    Nreq = size(Treq,1);
                    RequestCounter = 0;
                    for Ireq=1:1:Nreq
                        RequestCounter = RequestCounter + 1;
                        ID     = Treq.request_id(Ireq);
                        UserID = Treq.user_id(Ireq);
                        RA     = Treq.ra(Ireq);
                        Dec    = Treq.dec(Ireq);

                        % if Treq.checkexisting(Ireq)
                        %     % Check if forced photometry within 1'' was
                        %     % already requested in the past
                        % 
                        %     % FFU (maybe update range of JD...)
                        %     ReDo = true;
                        % else
                        %     % Re do without checking
                        %     ReDo = true;
                        % end
                        ReDo = true;

                        Msg = sprintf('New request found in table: %s',Obj.TableRequest);                    
                        Obj.writeLogMessage(Msg, 'Info', HostName);
                        Tstart = datetime('now');

                        if ReDo
                            if Treq.mountnum==0
                                Tvisit = searchTarget(Obj, RA, Dec, 'FieldID',Treq.fieldid(Ireq), 'CamNum',Treq.camnum(Ireq), 'CropID',Treq.cropid(Ireq), 'StartJD',Treq.jd_start(Ireq), 'EndJD',Treq.jd_end(Ireq));
                            else
                                Tvisit = searchTarget(Obj, RA, Dec, 'FieldID',Treq.fieldid(Ireq), 'MountNum',Treq.mountnum(Ireq), 'CamNum',Treq.camnum(Ireq), 'CropID',Treq.cropid(Ireq), 'StartJD',Treq.jd_start(Ireq), 'EndJD',Treq.jd_end(Ireq));                                
                            end
                            
                            Nobs = size(Tvisit,1);
                            if Nobs>0

                                % FFU: chose max n_epoch_max latest lines
                                % from table
                                
                                if Nobs>Treq.n_epoch_max(Ireq)
                                    Tvisit = sortrows(Tvisit, 'jd_start', 'descend');
                                    Tvisit = Tvisit(1:Treq.n_epoch_max(Ireq),:);
                                    Nobs = size(Tvisit,1);
                                end

                                % execure forced phot
                                try
                                    if Treq.get_cutout(Ireq)
                                        [ForcedPhot, ~, ADc] = pipeline.last.phot.forcedPhotSubLAST(Treq(Ireq,:), RA, Dec, 'UseExistingRef',Treq.useexistingref(Ireq), 'ReSub',Treq.resub(Ireq), 'LoadNew',Treq.loadnew(Ireq), 'MaxIter',Treq.maxiter(Ireq));
                                        FlagNotEmpty = ~ForcedPhot.isemptyCatalog;
                                        ForcedPhot   = ForcedPhot(FlagNotEmpty);
                                        ADc          = ADc(FlagNotEmpty);
            
                                        % write stamps to dir of stamps
                                        PWD = pwd;
                                        cd(Obj.CutoutPath);
            
                                        DirName = sprintf('%d',ID);
                                        %mkdir(DirName);
                                        % FFU
            
                                        cd(PWD);
            
                                    else
                                        [ForcedPhot] = pipeline.last.phot.forcedPhotSubLAST(Tvisit, RA, Dec, 'UseExistingRef',Treq.useexistingref(Ireq), 'ReSub',Treq.resub(Ireq), 'LoadNew',Treq.loadnew(Ireq), 'MaxIter',Treq.maxiter(Ireq));
                                        FlagNotEmpty = ~ForcedPhot.isemptyCatalog;
                                        ForcedPhot   = ForcedPhot(FlagNotEmpty);
                                    end % if Treq.get_cutout(Ireq)
            
                                    % merge forced phot tables
                                    % Fixing a problem with New_FIELDID -
                                    % making into string...
                                    Nfp = numel(ForcedPhot);
                                    if Nfp>0 && tools.table.isColumn(ForcedPhot(1).Catalog, 'New_FIELDID')
                                        for Ifp=1:1:Nfp
                                            ForcedPhot(Ifp).Catalog.New_FIELDID = string(ForcedPhot(Ifp).Catalog.New_FIELDID);
                                        end
                                    end
                                    ForcedPhot  = ForcedPhot.merge('IsTable',true);
            
                                    % add meta data to ForcedPhot table
                                    Nphot = ForcedPhot.sizeCatalog;
                                    if isempty(Nphot)
                                        Nphot = 0;
                                    end
                                    if Nphot>0
        
                                        % calculate UPIX
        
        
                                        ForcedPhot.Catalog = addvars(ForcedPhot.Catalog, repmat(Treq.request_id(Ireq),Nphot,1),...
                                                                                         repmat(Treq.user_id(Ireq),Nphot,1),...
                                                                                         repmat(Treq.ra(Ireq),Nphot,1),...
                                                                                         repmat(Treq.dec(Ireq),Nphot,1),...
                                                                                         'NewVariableNames',{'request_id', 'user_id', 'request_ra', 'request_dec'});
                                        % Insert Healpix indices
                                        ForcedPhot.Catalog = db.util.insertHealpixIndex2table(ForcedPhot.Catalog, Args.insertHealPixArgs{:});
    
                                        % write output to TableOutput
                                        % Create TableOutput: forcedphotsub_output
                                        %    
                                        %       Index = {'INDEX ra_dec_index (ra, dec) TYPE minmax GRANULARITY 64', 'INDEX request_id_index request_id TYPE minmax GRANULARITY 32', 'INDEX user_id_index user_id TYPE minmax GRANULARITY 1','INDEX nside_partition_index nside_partition TYPE minmax GRANULARITY 16','INDEX nside_low_index nside_low TYPE minmax GRANULARITY 16','INDEX nside_high_index nside_high TYPE minmax GRANULARITY 16'}
                                        %       Obj.DB.createTable('forcedphotsub_output',ForcedPhot.Catalog, [], [], 'Index', Index,'OrderBy','request_id');
                                        %       [~,Error] = Obj.DB.query('DROP TABLE IF EXISTS forcedphotsub_output', 'IsExec',true)
                                      
                                        % Tout=Obj.DB.query('SELECT * FROM forcedphotsub_output')
    
                                        ErrorInsert = Obj.DB.insertCharDump(Obj.TableOutput, ForcedPhot.Catalog);
                                    else
                                        ErrorInsert = [];
                                    end
                                catch ME
                                    ErrorInsert = 'forcedphot failed';
                                end
                                
        
                                % update status
                                % NOTE: TableRequest must be of type: ReplacingMergeTree
                                % otherwise updates are not possible.
                                if isempty(ErrorInsert)
                                    if Nphot==0
                                        Treq.status(Ireq) = STATUS_NOOBS;  % ready
                                        Treq.nphot(Ireq)  = Nphot;
                                        % DEBUG: TT=Obj.DB.query('SELECT * FROM forcedphot_requests')
                                        Obj.DB.query(sprintf("ALTER TABLE %s UPDATE %s = %d, %s = %d WHERE request_id = %d AND user_id = %d", Obj.TableRequest, 'status', STATUS_NOOBS, 'nphot', Nphot, ID, UserID), 'IsExec',true);
                                        %ALTER TABLE my_table
                                        %UPDATE column1 = 'new_value'
                                        %WHERE id = 123;
                                    else
                                        Treq.status(Ireq) = STATUS_READY;  % ready
                                        Treq.nphot(Ireq)  = Nphot;
                                        % DEBUG: TT=Obj.DB.query('SELECT * FROM forcedphot_requests')
                                        Obj.DB.query(sprintf("ALTER TABLE %s UPDATE %s = %d, %s = %d WHERE request_id = %d AND user_id = %d", Obj.TableRequest, 'status', STATUS_READY, 'nphot', Nphot, ID, UserID), 'IsExec',true);
                                    end

                                    Obj.DB.insertCharDump(Obj.TableRequest, Treq(Ireq,:));
                                else
                                    Nphot = 0;
                                    % write to log - change status to -1
                                    Obj.DB.query(sprintf("ALTER TABLE %s UPDATE %s = '%d', %s = '%d' WHERE request_id = %d AND user_id = %d", Obj.TableRequest, 'status', STATUS_FAILED, 'nphot', Nphot, ID, UserID), 'IsExec',true);
                                    %Obj.DB.query(sprintf("ALTER TABLE %s DELETE WHERE id = %d", Obj.TableRequest, ID), 'IsExec',true);
                                    Treq.status(Ireq) = STATUS_FAILED;  % failed
                                    Treq.nphot(Ireq)  = 0;
                                    %Obj.DB.insertCharDump(Obj.TableRequest, Treq(Ireq,:));
                                end % if isempty(ErrorInsert)
                            else
                                Nphot = 0;

                                % write to log - change status to 2
                                Obj.DB.query(sprintf("ALTER TABLE %s UPDATE %s = '%d', %s = '%d' WHERE request_id = %d AND user_id = %d", Obj.TableRequest, 'status', STATUS_NOOBS, 'nphot', Nphot, ID, UserID), 'IsExec',true);
                                %Obj.DB.query(sprintf("ALTER TABLE %s DELETE WHERE id = %d", Obj.TableRequest, ID), 'IsExec',true);
                                Treq.status(Ireq) = STATUS_NOOBS;  % not found
                                Treq.nphot(Ireq)  = 0;
                                %Obj.DB.insertCharDump(Obj.TableRequest, Treq(Ireq,:));

                            end % if Nobs>0
                            TotNphot = TotNphot + Nphot;
                        end % if ReDo
                        RunTime = datetime('now') - Tstart;
                        Msg = sprintf('Finished - Run time: %6.2f [s] for %d data points', seconds(RunTime), Nphot);                    
                        Obj.writeLogMessage(Msg, 'Info', HostName);
                        
                        TotNreq = TotNreq + Nreq;
                    end % for Ireq=1:1:Nreq
                   
                end % if ~isempty(Treq)

            end

        end
    end

    methods % access tools to DB
        function Treq=selectRequest(Obj, Args)
            % Select entries from request_table using optional constraints
            %   Accept constrainst on user_id, request_id, and ra and dec.
            %   If no constraints, then return all entries.
            % Input  : - self
            %          * ...,key,val,...
            %            'UserID' - user_id constraints. Default is [].
            %            'RequestID' - request_id constraints.
            %                   Default is [].
            %            'RA', 'Dec' - Constraints on 'ra','dec' [deg].
            %                   or 'RA' contains object name.
            %                   Default is [].
            %            'CooShift' - Box search radius.
            %                   Default is 0.001 deg.
            %                   Current version will fail near RA=0,
            %                   Dec=poles.
            %
            % Output : - Table of selected forced phot request entries from DB.
            % Author : Eran Ofek (Sep 2025)
            % Example: FFS=pipeline.last.pipes.ForcedPhotServer(DB);
            %          Treq=FFS.selectRequest; % get all requests
            %          Treq=FFS.selectRequest('UserID',0)
            %          Treq=FFS.selectRequest('UserID',0, 'RA',260.57096, 'Dec',58.86383) 

            arguments
                Obj
                Args.UserID     = [];
                Args.RequestID  = [];
                Args.RA         = [];
                Args.Dec        = [];
                Args.CooShift   = 0.001;
            end

            Counter = 0;

            if isempty(Args.UserID)
                %
                UserID = '';
            else
                if Counter==0
                    UserID = sprintf('user_id=%d', Args.UserID);
                else
                    UserID = sprintf('AND user_id=%d', Args.UserID);
                end
                Counter = Counter + 1;
            end
            if isempty(Args.RequestID)
                %
                RequestID = '';
            else
                if Counter==0
                    RequestID = sprintf('request_id=%d', Args.RequestID);
                else
                    RequestID = sprintf('AND request_id=%d', Args.RequestID);
                end
                Counter = Counter + 1;
            end
            if ~isempty(Args.RA)
                [RA, Dec] = celestial.convert.cooResolve(Args.RA, Args.Dec);
                WhereCoo = sprintf('(ra BETWEEN %10.6f AND %10.6f) AND (dec BETWEEN %10.6f AND %10.6f)', RA-Args.CooShift, RA+Args.CooShift, Dec-Args.CooShift, Dec+Args.CooShift);
                if Counter>0
                    WhereCoo = sprintf('AND %s', WhereCoo);
                end
                Counter = Counter + 1;

            else
                WhereCoo = '';
            end
            

            if Counter==0
                Query = sprintf('SELECT * FROM %s', Obj.TableRequest);
            else
                Query = sprintf('SELECT * FROM %s WHERE %s %s %s', Obj.TableRequest, UserID, RequestID, WhereCoo);
            end

            Treq = Obj.DB.query(Query);


        end
    
        function Tobs=selectObservations(Obj, Args)
            % Select entries from forcedphotsub_output using optional constraints
            %   First search using selectRequest and then select from
            %   putput table.
            %   Accept constrainst on user_id, request_id, and ra and dec.
            %   If no constraints, then return all entries.
            % Input  : - self
            %          * ...,key,val,...
            %            'UserID' - user_id constraints. Default is [].
            %            'RequestID' - request_id constraints.
            %                   Default is [].
            %            'RA', 'Dec' - Constraints on 'ra','dec' [deg].
            %                   or 'RA' contains object name.
            %                   Default is [].
            %            'CooShift' - Box search radius.
            %                   Default is 0.001 deg.
            %                   Current version will fail near RA=0,
            %                   Dec=poles.
            %
            % Output : - Table of selected forced phot observations entries from DB.
            % Author : Eran Ofek (Sep 2025)
            % Example: FFS=pipeline.last.pipes.ForcedPhotServer(DB);
            %          Tobs=FFS.selectObservations('UserID',0, 'RequestID',5)
            %          Tobs=FFS.selectObservations('UserID',0, 'RA',260.57096, 'Dec',58.86383) 

            arguments
                Obj
                Args.UserID     = [];
                Args.RequestID  = [];
                Args.RA         = [];
                Args.Dec        = [];
                Args.CooShift   = 0.001;
            end

            Treq = selectRequest(Obj, 'UserID',Args.UserID, 'RequestID',Args.RequestID, 'RA',Args.RA, 'Dec',Args.Dec, 'CooShift',Args.CooShift);

            Nline = size(Treq,1);
            switch Nline
                case 1
                    Query = sprintf('SELECT * FROM %s WHERE user_id=%d AND request_id=%d', Obj.TableOutput, Treq.user_id, Treq.request_id);
                    Tobs = Obj.DB.query(Query);
                case 0
                    Tobs = [];
                otherwise
                    Treq
                    error('Multiple entries were found');
            end

        end
    end


    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
