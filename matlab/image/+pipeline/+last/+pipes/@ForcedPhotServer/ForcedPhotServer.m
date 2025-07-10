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
                    T = Obj.DB.query(sprintf("SELECT * FROM visit_images WHERE fieldid LIKE '%s' AND camnum=%d AND cropid=%d AND jd_start>%f AND jd_start<%f",Args.FieldID, Args.CamNum, Args.CropID, Args.StartJD, Args.EndJD));
                else
                    T = Obj.DB.query(sprintf("SELECT * FROM visit_images WHERE fieldid LIKE '%s' AND mountnum=%d AND camnum=%d AND cropid=%d AND jd_start>%f AND jd_start<%f",Args.FieldID, Args.MountNum, Args.CamNum, Args.CropID, Args.StartJD, Args.EndJD));
                end
            else
                T = pipeline.last.queryDB.searchVisitsByCoo(RA, Dec, 'DB', Obj.DB, Args.searchVisitsByCooArgs{:});
                T = T{1};
            end
     
        end

        function createRequestsTable(Obj)
            % Re-create forced photometry requests table
            % Input  : - self.
            % Output : null
            % Author : Eran Ofek (Jul 2025)


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

                Args.UseExistingRef    = false;
                Args.ReSub             = false;
                Args.LoadNew           = false;
                Args.MaxIter           = 0;

                Args.insertHealPixArgs = {'ColRA','request_ra','ColDec','request_dec'};
            end
            STATUS_WAITING = 0;
            STATUS_READY   = 1;
            STATUS_FAILED  = -1;
            HostName       = tools.os.get_computer;

            Obj = pipeline.last.pipes.ForcedPhotServer(Args.DB);

            LoopInd = 0;
            RequestCounter = 0;
            while true
                pause(Args.PauseTime);
                LoopInd = LoopInd + 1;
                if mod(LoopInd,100)==0
                    [LoopInd, RequestCounter]
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
                % Obj.DB.insertCharDump('forcedphot_requests',table(2,0,262.72824, 66.68995, 2460000,2470000,"1718",1,1,3,14, 1,'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid', 'nodenumb', 'mountnum', 'camnum', 'cropid', 'loadnew'}))
                % Obj.DB.insertCharDump('forcedphot_requests',table(3,0,260.5709627, 58.8638455, 2450000,2470000,"1632",1,3,1,10, 1,'VariableNames',{'request_id','user_id','ra','dec','jd_start','jd_end','fieldid', 'nodenumb', 'mountnum', 'camnum', 'cropid', 'loadnew'}))
                % 
                % INSERT INTO forcedphot_requests (request_id, user_id, ra, dec, jd_start, jd_end, fieldid, nodenumb, mountnum, camnum, cropid, loadnew) VALUES  ( 2, 0, 262.728240000000028, 66.6899499999999961, 2460000, 2470000, '1718', 1, 1, 3, 14, 1 )
                % user_id: 0 - tests, 1 - last pipe, 2 - cast, 3 - webaccess

                Treq = Obj.DB.query(sprintf("SELECT * FROM %s WHERE status=%d", Obj.TableRequest, STATUS_WAITING));
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
                            if Treq.mountnum>0
                                Tvisit = searchTarget(Obj, RA, Dec, 'FieldID',Treq.fieldid(Ireq), 'MountNum',Treq.mountnum(Ireq), 'CamNum',Treq.camnum(Ireq), 'CropID',Treq.cropid(Ireq), 'StartJD',Treq.jd_start(Ireq), 'EndJD',Treq.jd_end(Ireq));
                            else
                                Tvisit = searchTarget(Obj, RA, Dec, 'FieldID',Treq.fieldid(Ireq), 'CamNum',Treq.camnum(Ireq), 'CropID',Treq.cropid(Ireq), 'StartJD',Treq.jd_start(Ireq), 'EndJD',Treq.jd_end(Ireq));
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
                                end
        
                                % merge forced phot tables
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
                                  
                                    ErrorInsert = Obj.DB.insertCharDump(Obj.TableOutput, ForcedPhot.Catalog);
                                else
                                    ErrorInsert = [];
                                end
        
                                
        
                                % update status
                                % NOTE: TableRequest must be of type: ReplacingMergeTree
                                % otherwise updates are not possible.
                                if isempty(ErrorInsert)
                                    Treq.status(Ireq) = STATUS_READY;  % ready
                                    Treq.nphot(Ireq)  = Nphot;
                                    % DEBUG: TT=Obj.DB.query('SELECT * FROM forcedphot_requests')
                                    Obj.DB.query(sprintf("ALTER TABLE %s UPDATE %s = '%d', %s = '%d' WHERE request_id = %d AND user_id = %d", Obj.TableRequest, 'status', STATUS_READY, 'nphot', Nphot, ID, UserID), 'IsExec',true);
                                    %ALTER TABLE my_table
                                    %UPDATE column1 = 'new_value'
                                    %WHERE id = 123;

                                    Obj.DB.insertCharDump(Obj.TableRequest, Treq(Ireq,:));
                                else
                                    % write to log - change status to -1
                                    Obj.DB.query(sprintf("ALTER TABLE %s UPDATE %s = '%d', %s = '%d' WHERE request_id = %d AND user_id = %d", Obj.TableRequest, 'status', STATUS_FAILED, 'nphot', Nphot, ID, UserID), 'IsExec',true);
                                    %Obj.DB.query(sprintf("ALTER TABLE %s DELETE WHERE id = %d", Obj.TableRequest, ID), 'IsExec',true);
                                    Treq.status(Ireq) = STATUS_FAILED;  % failed
                                    Treq.nphot(Ireq)  = 0;
                                    %Obj.DB.insertCharDump(Obj.TableRequest, Treq(Ireq,:));
                                end
                            else
                                Nphot = 0;
                            end % if Nobs>0
                        end % if ReDo
                        RunTime = datetime('now') - Tstart;
                        Msg = sprintf('Finished - Run time: %6.2f [s] for %d data points', seconds(RunTime), Nphot);                    
                        Obj.writeLogMessage(Msg, 'Info', HostName);
                        
                    end % for Ireq=1:1:Nreq
                   
                end % if ~isempty(Treq)

            end

        end
    end


    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
