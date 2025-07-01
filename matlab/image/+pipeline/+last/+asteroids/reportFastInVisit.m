function reportFastInVisit(Args)
    % Search for fast asteroids to report and generate reports
    %   The function search for new entries in the last.fast_asteroids DB
    %   and generate a report for these asteroids.
    % Input  : * ...,key,val,... 
    %            See code for options.
    % Output : null
    % Author : Eran Ofek (2025 Jul) 
    % Example: pipeline.last.asteroids.reportFastInVisit

    arguments
    
        Args.IngestionJDRange  = [0 2460810];  % JD range of ingestion_time to report
        Args.MaxNumAstIndex    = 2;
        Args.DB                = [];
        Args.Comment           = ["LAST observations - Mounts 1 to 10", "Each triplet is based on a linear fit to 20 images of 20s"];
        Args.Measurer          = ["L. Pipeline", "E. Ofek"];
        Args.Submitter         = ["D. Polishook"];
    end



    %% Report fast moving
    % till IngestionTime: 2460810

    OrbEl= celestial.OrbitalEl.loadSolarSystem('merge');
    IN = celestial.INPOP.init;
    
    ModelTime = [-2.5 0 2.5]./1440;
    
    RAD = 180./pi;

    if isempty(Args.DB)
        DB = db.Db;
        DB.connect;
        DB.useDB('last');
    else
        DB = Args.DB;
    end

    QueryStr = sprintf('SELECT * FROM fastmoving_asteroids WHERE propermotion>0.4 AND rms<0.3 AND (propermotion/rms)>8 AND (sn*rms)<30 AND insertion_time_jd>%-13.3f AND insertion_time_jd<=%-13.3f',Args.IngestionJDRange(1), Args.IngestionJDRange(2));

    T = DB.query(QueryStr);

    % Remove fields which have >1 fast asteroids detection
    FlagAstInd = T.astindex>Args.MaxNumAstIndex;
    FlagID     = ismember(T.id, T.id(FlagAstInd));
    T          = T(~FlagID,:);
    Ncand      = size(T,1);
    CandFound  = false(Ncand,1);
    T          = sortrows(T, 'jd');
    
    UnVisit = unique([T.id, T.astindex],'rows');
    NunV    = size(UnVisit,1);


    Tout = struct('Data',cell(NunV,1));
    Nt = numel(ModelTime);
    Desig = strings(NunV.*Nt,1);
    for IunV=1:1:NunV
        [IunV, NunV]
        IndUnV = find(T.id==UnVisit(IunV,1) & T.astindex==UnVisit(IunV,2));
        Npt    = numel(IndUnV);
        DeltaT = range(T.jd(IndUnV));
        FitPar = imUtil.asteroids.fitMotion(T.jd(IndUnV), T.ra(IndUnV), T.dec(IndUnV));
        FitGC  = imUtil.asteroids.fitMotionGreatCircle(T.jd(IndUnV), T.ra(IndUnV), T.dec(IndUnV), 'ModelTime',ModelTime);
    
    
        VecJD     = [FitPar.RefJD-DeltaT.*0.4, FitPar.RefJD, FitPar.RefJD+DeltaT.*0.4];
        Njd       = numel(VecJD);
        Ones      = ones(1,Njd);
    
      
        
        Ones = ones(1,Nt);
        Tout(IunV).Data = [FitGC.ModelTime.' + FitGC.RefT; ...
                           FitGC.ModelLon0.'; ...
                           FitGC.ModelLat0.'; ...
                           FitGC.LonRMS.*3600.*Ones; ...
                           FitGC.LatRMS.*3600.*Ones; ...
                           FitGC.RMS.*3600.*Ones; ...
                           FitGC.N.*Ones; ...
                           FitGC.RateLonCos.*Ones; ...
                           FitGC.RateLon.*Ones; ...
                           FitGC.RateLat.*Ones; ...
                           T.mag(IndUnV(1)).*Ones; ...
                           T.sn(IndUnV(1)).*Ones; ...
                           T.distmp(IndUnV(1)).*Ones; ...
                           (1:Nt); ...
                           IunV.*Ones;...
                           UnVisit(IunV,1).*Ones;...
                           UnVisit(IunV,2).*Ones;...
                           T.insertion_time_jd(IndUnV(1)).*Ones];
    
    
        [Result] = searchMinorPlanetsNearPosition(OrbEl,FitGC.ModelTime(1) + FitGC.RefT, FitGC.ModelLon0(1), FitGC.ModelLat0(1), 3, 'INPOP',IN, 'ConeSearch',true);                    
        if Result.sizeCatalog==1
            Desig((IunV-1).*Nt+1:(IunV-1).*Nt+Nt) = Result.Catalog.Desig;
        end
        
    end

    OutTable = array2table([Tout.Data]', 'VariableNames',{'JD', 'RA', 'Dec', 'RA_RMS', 'Dec_RMS', 'RMS', 'N', 'RateLonCos', 'RateLon', 'RateLat', 'Mag', 'SN', 'DistMP', 'TripletInd', 'TrackletNumber', 'ID', 'AstIndex', 'IngestionTimeJD'});
    OutTable.Desig = Desig;
    OutTable.MagErr = max(0.02, 1.086./OutTable.SN);
    
    % Output file format (for all files)
    AFN = AstroFileName;
    AFN.ProjName = {"LAST",1,0,0};
    AFN.JD = Args.IngestionJDRange(2);
    AFN.julday2time;
    AFN.Counter=0;
    AFN.CCDID=0;
    AFN.CropID=0;
    AFN.Type="sci";
    AFN.Level="report";
    AFN.Product="Asteroids.Fast";
    AFN.FileType="xml";

    FileName = AFN.genFile;

    imUtil.asteroids.generateReportMPC_ADES(OutTable, FileName, 'Submitter',Args.Submitter, 'Measurer',Args.Measurer, 'ColJD','JD', 'ColRA','RA', 'ColDec','Dec', 'ColErrRA','RA_RMS', 'ColErrDec','Dec_RMS', 'ColMag','Mag', 'ColErrMag','MagErr', 'ColProvID','Desig', 'ColSN', 'SN', 'ColTrkSub','TrackletNumber', 'ColExpTime',20, 'Comment',Args.Comment);

   
    % log/summary

    AFN.FileType="mat";
    AFN.Type = "log";
    AFN.FileType="mat";

    SummaryFileName = AFN.genFile;
    save('-v7.3', SummaryFileName, 'OutTable');
    
end







