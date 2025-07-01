% TransientClassify - 
%
% Example:
%

classdef TransientClassify < Component
    % 
            
    properties       
        %
        DB           = [];
        
        DbName       = 'last';

        TableLevel1  = [];
        TableLevel2  = [];

        CutoutPath   = '/lastdata/forcedphotsub'
        ObsCoo       = [35 30 415];  % [deg deg m]

    end
    
    properties (Constant, Hidden)
        User         = 'euclid/root';
    end

    methods % Constructor
       
        function Obj = TransientClassify(DB)
            % Constructor for TransientClassify
        
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
            
        end
        
    end
    
    methods % setter/getters
       
    end
      
    methods
        function demon(Obj, Args)
            %

            arguments
                Obj
                Args.AssocRadius  = 1.5;  % [arcsec]
                Args.LoopPause = 1;   % [seconds]
                Args.ColStatus = 'status';
                Args.ColRA     = 'ra';
                Args.ColDec    = 'dec';
                Args.ColAccumCosX = 'accum_cosx';
                Args.ColAccumCosY = 'accum_cosy';
                Args.ColAccumCosZ = 'accum_cosz';
                Args.ColCounter   = 'counter';
                Args.ColLastMag   = 'last_mag';

                Args.ColMag       = 'mag';
                Args.ColLastSN    = 'last_sn';
                Args.ColSN        = 'sn';
                Args.ColJD        = 'jd';
                Args.ColFirstJD   = 'jd_first';
                Args.ColLastJD    = 'jd_last';
            end
            RAD        = 180./pi;
            ARCSEC_DEG = 3600;


            Cont = true;
            while Cont
                pause(Args.LoopPause);

                % Query table for unclassified transients | moving?
                LatestIngestionTime = celestial.time.julday();
                % T_Level1 = DB.query

                IndNew = find(T_Level1.(Args.ColStatus) == 0, 1, 'first');
                T_Level1 = T_Level1(IndNew,:);
                RA  = T_Level1.(Args.ColRA);
                Dec = T_Level1.(Args.ColDec);


                % Query Level 2 unique transients (only if was updated)
                % query by position +/- a few arcseconds
                T_Unique

                Dist = celestial.coo.sphere_dist_fast(RA./RAD, Dec./RAD, T_Unique.(Args.ColRA)./RAD, T_Unique.(Args.ColDec)./RAD ).*RAD.*ARCSEC_DEG;
                Iun  = find(Dist<Args.AssocRadius);
                if numel(Iun)>1
                    [~,Iun] = min(Dist);
                    NinRAssocadius = numel(Iun);
                else
                    NinAssocRadius = 1;
                end
                if isempty(Iun)
                    % New source is not associated with a unique source

                    % add source to unique sources with counter=1

                else
                    % update information on unique source:
                    % update: counter, accum_cosx, accum_cosy, accum_cosz,
                    % last_mag, last_sn, jd_start, jd_end, NinAssocRadius
                    [CDX, CDY, CDZ] = celestial.coo.coo2cosined(RA, Dec);
                    T_Unique = T_Unique(Iun,:);

                    T_Unique.(Args.ColAccumCosX) = T_Unique.(Args.ColAccumCosX) + CDX;
                    T_Unique.(Args.ColAccumCosY) = T_Unique.(Args.ColAccumCosY) + CDY;
                    T_Unique.(Args.ColAccumCosZ) = T_Unique.(Args.ColAccumCosZ) + CDZ;
                    T_Unique.(Args.ColCounter)   = T_Unique.(Args.ColCounter)   + 1;
                    T_Unique.(Args.ColLastMag)   = T_Level1.(Args.ColMag);
                    T_Unique.(Args.ColLastSN)    = T_Level1.(Args.ColSN);
                    if T_Level1.(Args.ColJD)>T_Unique.(Args.ColLastJD)
                        T_Unique.(Args.ColLastJD) = T_Level1.(Args.ColJD);
                    end
                    if T_Level1.(Args.ColJD)<T_Unique.(Args.ColFirstJD)
                        T_Unique.(Args.ColFirstJD) = T_Level1.(Args.ColJD);
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
