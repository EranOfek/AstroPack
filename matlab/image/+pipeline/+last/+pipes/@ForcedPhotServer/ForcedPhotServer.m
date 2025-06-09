% ForcedPhotServer - 
%
% Example:
%

classdef ForcedPhotServer < Component
    % 
            
    properties       
        %
        DB           = [];
        User         = 'euclid/root';
        DbName       = 'last';

        TableRequest = 'forcedphot_requests';   % must be of type: ReplacingMergeTree
        TableOutput  = 'forcedphotsub_output';

        CutoutPath   = '/lastdata/forcedphotsub'
        ObsCoo       = [35 30 415];  % [deg deg m]

    end
    
    
    
    methods % Constructor
       
        function Obj = ForcedPhotServer(DB)
            % Constructor for ForcedFphotServer
        
            arguments
                DB = [];
            end
            
            if isempty(DB)
                Obj.DB = db.Db;
                Obj.DB.User = User;
                Obj.DB.connect;
                Obj.DB.useDB(Obj.DbName);
            else
                Obj.DB = DB;
            end
            
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

            if ~isempty(Args.FieldID)
                if isempty(Args.MountNum)
                    T = Obj.DB.query(sprintf("SELECT * FROM visit_images WHERE fieldid LIKE '%s' AND camnum=%d AND cropid=%d AND jd_start>%f AND jd_start<%f",Args.FieldID, Args.CamNum, Args.CropID, Args.StartJD, Args.EndJD));
                else
                    T = Obj.DB.query(sprintf("SELECT * FROM visit_images WHERE fieldid LIKE '%s' AND mountnum=%d AND camnum=%d AND cropid=%d AND jd_start>%f AND jd_start<%f",Args.FieldID, Args.MountNum, Args.CamNum, Args.CropID, Args.StartJD, Args.EndJD));
                end
            else
                T = pipeline.last.queryDB.searchVisitsByCoo(RA,Des, 'DB', Obj.DB, Args.searchVisitsByCooArgs{:});
                T = T{1};
            end
     
        end
    
        
    end

    methods (Static) % demon
        function demon(Args)
            %

            arguments
                Args.PauseTime = 1;
                Args.UseExistingRef    = false;
                Args.ReSub             = false;
                Args.LoadNew           = false;
                Args.MaxIter           = 0;

            end
            STATUS_WAITING = 0;
            STATUS_READY   = 1;
            STATUS_FAILED  = -1;

            while true
                pause(Args.PauseTime);

                % search for new request
                % TableRequest contains columns:
                %   request_id, user_id, ra, dec, subtraction (default is true), status (created with default=0), nphot (number of data points added), jd_start, jd_end, fieldid,
                %   nodenum, mountnum, camnum, cropid, useexistingref (default true), resub (default false),
                %   loadnew (default false), maxiter (default is 0),
                %   get_cutout (default is 0), insertion_time (default is
                %   now)

                % To create this table:
                %       VarNames = {'request_id', 'user_id', 'ra', 'dec', 'subtraction', 'status', 'nphot', 'jd_start', 'jd_end', 'fieldid', 'nodenum', 'mountnum', 'camnum', 'cropid','useexistingref', 'resub', 'loadnew', 'maxiter', 'get_cutout', 'insertion_time'};
                %       DB.createTable('forcedphot_requests',VarNames, ["UInt64","UInt8","Float64", "Float64","UInt8","UInt8","UInt32","Float64","Float64","String","UInt8", "UInt8","UInt8","UInt8", "UInt8", "UInt8", "UInt8", "UInt8", "UInt8", "DateTime64(3,'UTC')"], {[],0,[],[],1,0,[],[],[],[],1,[],[],[],1,0,1,0,0,'now64(3)'}, 'Index', {'INDEX ra_index ra TYPE minmax GRANULARITY 1', 'INDEX dec_index dec TYPE minmax GRANULARITY 1', 'INDEX request_id_index request_id TYPE minmax GRANULARITY 1', 'INDEX user_id_index user_id TYPE minmax GRANULARITY 1'},'OrderBy','insertion_time');
                %

                Treq = Obj.DB.query(sprintf("SELECT * FROM %s WHERE status=%d", Obj.TableRequest, STATUS_WAITING));
                if ~isempty(Treq)
                    % search target in visits
                    Nreq = size(Treq,1);
                    for Ireq=1:1:Nreq
                        ID  = Treq.request_id(Ireq);
                        RA  = Treq.ra(Ireq);
                        Dec = Treq.dec(Ireq);
                        if Treq.mountnum>0
                            Tvisit = searchTarget(Obj, RA, Dec, 'FieldID',Treq.fieldid(Ireq), 'MountNum',Treq.mountnum(Ireq), 'CamNum',Treq.camnum(Ireq), 'CropID',Treq.cropid(Ireq), 'StartJD',Treq.jd_start(Ireq), 'EndJD',Treq.jd_end(Ireq));
                        else
                            Tvisit = searchTarget(Obj, RA, Dec, 'FieldID',Treq.fieldid(Ireq), 'CamNum',Treq.camnum(Ireq), 'CropID',Treq.cropid(Ireq), 'StartJD',Treq.jd_start(Ireq), 'EndJD',Treq.jd_end(Ireq));
                        end
                        
                        % execure forced phot
                        if Treq.get_cutout(Ireq)
                            [ForcedPhot, ~, ADc] = pipeline.last.phot.forcedPhotSubLAST(T, RA, Dec, 'UseExistingRef',Treq.useexistingref(Ireq), 'ReSub',Treq.resub(Ireq), 'LoadNew',Treq.loadnew(Ireq), 'MaxIter',Treq.maxiter(Ireq));
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
                            [ForcedPhot] = pipeline.last.phot.forcedPhotSubLAST(T, RA, Dec, 'UseExistingRef',Treq.useexistingref(Ireq), 'ReSub',Treq.resub(Ireq), 'LoadNew',Treq.loadnew(Ireq), 'MaxIter',Treq.maxiter(Ireq));
                            FlagNotEmpty = ~ForcedPhot.isemptyCatalog;
                            ForcedPhot   = ForcedPhot(FlagNotEmpty);
                        end

                        % merge forced phot tables
                        ForcedPhot  = ForcedPhot.merge('IsTable',true);

                        % add meta data to ForcedPhot table
                        Nphot = ForcedPhot.sizeCatalg;
                        if Nphot>0
                            ForcedPhot.Catalog = addvars(ForcedPhot.Catalog, Treq.request_id(Ireq), Treq.user(Ireq), Treq.ra(Ireq), Treq.dec(Ireq), 'NewVariableNames',{'request_id','user_id', 'request_ra', 'request_dec'});

                            % write output to TableOutput
                            ErrorInsert = Obj.DB.insertCharDump(TableOutput, ForcedPhot.Catalog);
                        else
                            ErrorInsert = [];
                        end

                        

                        % update status
                        % NOTE: TableRequest must be of type: ReplacingMergeTree
                        % otherwise updates are not possible.
                        if isempty(ErrorInsert)
                            Treq.status(Ireq) = STATUS_READY;  % ready
                            Treq.nphot(Ireq)  = Nphot;
                            Obj.DB.insertCharDump(TableRequest, Treq(Ireq,:));
                        else
                            % write to log - change status to -1
                            Obj.DB.exec(sprintf("ALTER TABLE %s DELETE WHERE id = %d", Obj.TableRequest, ID));
                            Treq.status(Ireq) = STATUS_FAILED;  % failed
                            Treq.nphot(Ireq)  = 0;
                            Obj.DB.insertCharDump(TableRequest, Treq(Ireq,:));
                        end

                    end
    
                    
                end

            end

        end
    end


    %----------------------------------------------------------------------
    % Unit test
    methods(Static)
        Result = unitTest()
    end
    
end
