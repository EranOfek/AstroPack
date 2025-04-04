function [Result,Table,AstIndex] = fitFastMotion(Obj, Args)
    % Search fast moving asteroids in MatchedSources object.
    %     See details / using: imUtil.asteroids.fitFastMotion
    % Input  : - A MatchedSources object.
    %          * ...,key,val,... 
    %            'FieldRA'  - Field containing RA. Default is 'RA'.
    %            'FieldDec' - Default is 'Dec'.
    %            'FieldMag' - Default is 'MAG_PSF'.
    %            'FieldFlag' - Default is 'FLAGS'.
    %            'BitDict' - BitDictionary object. Default is BitDictionary
    %            'FlagsList' - Bad flags to remove.
    %                   Default is {'NearEdge','Saturated','NaN','Negative'};
    %
    %            'MaxNdet'- Max. number of detections of source "in the smae position"
    %                   over all epochs. Default is 7.
    %            'MaxTimeDiff' - Max. time range for MaxNdet (days).
    %                   Default is 7.*20./86400.
    %            'MaxDist' - Maximum distance between points to fit linear
    %                   motion (units are in CooUnits').
    %                   Default is 0.03.
    %            'CooUnits' - RA, Dec, MaxDist units. Default is 'deg'.
    %            'NptFit' - Number of points to fit in each RANSAC
    %                   simulation. Default is 3.
    %            'MinNpt' - Min. Number of points in the best fit solution
    %                   of a moving source.
    %                   Default is 8.
    %            'ThresholdDist' - Threshold distance of points in
    %                   best fit solution from the linear motion.
    %                   Default is 3 arcsec.
    %            'AstIndex' - Last AsteroidIndex used. This is the internal
    %                   number of the asteroid in the report.
    %                   Default is 0.
    %            'OutType' - Type of second output argument:
    %                   'table'|'AstroCatalog'. Default is 'AstroCatalog'.
    %
    % Output : - A structure array with element per fast moving source
    %            found. See details in imUtil.asteroids.fitFastMotion
    %            The .Tag field specify the index of the MatchedSources
    %            object element.
    %          - Table of all asteroid candidates observations', with columns:
    %            {'JD', 'RA', 'Dec', 'FitRA', 'FitDec', 'RMS', 'Mag', 'SN', 'Flags', 'AstIndex'};
    %          - AstIndex of latest asteroid found.
    % Author : Eran Ofek (2025 Mar) 
    % Example: R=lcUtil.fitFastMotion(MS);

    arguments
        Obj
        Args.FieldMag                 = 'MAG_PSF';
        Args.BitDict                  = BitDictionary;
        Args.FieldFlag                = 'FLAGS';
        Args.FlagsList                = {'NearEdge','Saturated','NaN','Negative'};
        Args.FieldSN                  = 'SN_3';

        Args.MaxNdet           = 7;
        Args.MaxTimeDiff       = 7.*20./86400;  % days
        Args.MaxDist           = 0.03;  % deg
        Args.CooUnits          = 'deg';
        Args.NptFit            = 3;
        Args.MinNpt            = 8;
        Args.ThresholdDist     = 3;  % arcsec
        Args.AstIndex          = 0;

        Args.OutType           = 'AstroCatalog';  % 'table'|'AstroCatalog'

        Args.INPOP             = celestial.INPOP.init;
        Args.OrbEl             = [];

        Args.Visit             = "";

    end 
    RAD = 180./pi;
     
    
    ColNames1 = {'JD', 'RA', 'Dec', 'FitRA', 'FitDec', 'RMS', 'Mag', 'SN', 'Flags', 'AstIndex', 'ProperMotion', 'DistMP', 'Nstars', 'NearestStarDist', 'NearestStarMag', 'CropID'};
    ColNames2 = {'ProjName', 'FieldID', 'Visit'};
    ColNames3 = {'ID', 'ObsNumber'};
    ColNames  = [ColNames1, ColNames2, ColNames3];

    EmptyCell = cell(1,numel(ColNames));
    Table    = table(EmptyCell{:});
    %Table.Properties.VariableNames = ColNames;

    % remove bad flags
    FlagBad = searchFlags(Obj, 'BitDic',Args.BitDict, 'PropFlags',Args.FieldFlag, 'FlagsList',Args.FlagsList, 'UseSrcData',true);

    
    K      = 0;
    Nobj   = numel(Obj);
    Result = [];
    for Iobj=1:1:Nobj
        % add SrcData




        [Tmp] = imUtil.asteroids.fitFastMotion(Obj(Iobj).JD, Obj(Iobj).Data.RA, Obj(Iobj).Data.Dec,...
                                                    'FlagGood',~FlagBad,...
                                                    'Tag',Iobj,...
                                                    'DimEpoch',Obj.DimEpoch,...
                                                    'MaxNdet',Args.MaxNdet,...
                                                    'MaxTimeDiff',Args.MaxTimeDiff,...
                                                    'MaxDist',Args.MaxDist,...
                                                    'CooUnits',Args.CooUnits,...
                                                    'NptFit',Args.NptFit,...
                                                    'MinNpt',Args.MinNpt,...
                                                    'ThresholdDist',Args.ThresholdDist);
        
        
        % Match to known asteroids
        Ntmp = numel(Tmp);
        DistMP = nan(Ntmp,1);
        for Itmp=1:1:Ntmp
            MedRA  = median(Tmp(Itmp).RA);
            MedDec = median(Tmp(Itmp).Dec);
            MedJD  = median(Tmp(Itmp).JD);
            TmpAC = AstroCatalog({[MedRA, MedDec]}, 'ColNames',{'RA','Dec'});
            [OnlyMP, AstCat, AC1] = imProc.match.match2solarSystem(TmpAC, 'JD',MedJD, 'GeoPos',[], 'OrbEl',Args.OrbEl, 'SearchRadius',10, 'INPOP',Args.INPOP, 'RA',MedRA, 'Dec',MedDec, 'FOV_Radius',0.1, 'InCooUnits','deg');
            DistMP(Itmp) = AC1.Catalog(1,3);

            % search for GAIA stars
            Ndet = numel(Tmp(Itmp).RA);
            for Idet=1:1:Ndet
                [CatG,~,~,DistStar] = catsHTM.cone_search('GAIADR3', Tmp(Itmp).RA(Idet)./RAD, Tmp(Itmp).Dec(Idet)./RAD, 10, 'OutType','AstroCatalog');
                if CatG.sizeCatalog==0
                    NearestStarDist = NaN;
                    NearestStarMag  = NaN;
                    Nstars          = 0;
                else
                    Nstars = CatG.sizeCatalog;
                    [NearestStarDist, Inear] = min(DistStar.*RAD.*3600);
                    NearestStarMag = CatG.Table.phot_bp_mean_mag(Inear);
                end
                Tmp(Itmp).Nstars(Idet) = Nstars;
                Tmp(Itmp).NearestStarDist(Idet) = NearestStarDist;
                Tmp(Itmp).NearestStarMag(Idet)  = NearestStarMag;

            end


        end

        if Iobj==1
            Result = Tmp(:);
        else
            Result = [Result; Tmp(:)];
        end

        if nargout>1
            switch lower(Args.OutType)
                case 'astrocatalog'
                    % prep AstroCatalog output (element per asteroid)

                    FN = FileNames.generateFromFileName(Obj(Iobj).FileName);
    
                    ProjName = string(FN.ProjName{1});
                    FieldID  = string(FN.FieldID{1});
                    CropID   = FN.CropID(1);
                    Nfiles   = numel(FN.Time);
                    Visit    = string(Args.Visit);

                    AddColData = [ProjName, FieldID, Visit];

                    Ntmp = numel(Tmp);
                    for Itmp=1:1:Ntmp
                        
                        Args.AstIndex = Args.AstIndex + 1;
                        K = K + 1;
                        if K==1
                            Table = AstroCatalog;
                        end
                        Nobs = Tmp(Itmp).Npt;
                        % {'JD', 'RA', 'Dec', 'FitRA', 'FitDec', 'RMS', 'Mag', 'SN', 'Flags', 'AstIndex', 'ProperMotion', 'DistMP', 'CropID', Nstars, NearestStarDist, NearestStarMag}
                        Table(K).Catalog = table(Tmp(Itmp).JD,...
                                                 Tmp(Itmp).RA,...
                                                 Tmp(Itmp).Dec,...
                                                 Tmp(Itmp).FitRA,...
                                                 Tmp(Itmp).FitDec,...
                                                 Tmp(Itmp).RMS.*ones(Nobs,1),...
                                                 Obj(Iobj).Data.(Args.FieldMag)(Tmp(Itmp).Ind),...
                                                 Obj(Iobj).Data.(Args.FieldSN)(Tmp(Itmp).Ind),...
                                                 uint32(Obj(Iobj).Data.(Args.FieldFlag)(Tmp(Itmp).Ind)),...
                                                 Args.AstIndex.*ones(Nobs,1),...
                                                 Tmp(Itmp).ProperMotion.*ones(Nobs,1),...
                                                 DistMP(Itmp).*ones(Nobs,1),...
                                                 Tmp(Itmp).Nstars(:),...
                                                 Tmp(Itmp).NearestStarDist(:),...
                                                 Tmp(Itmp).NearestStarMag(:),...
                                                 CropID.*ones(Nobs,1));
                        Table(K).Catalog.Properties.VariableNames = ColNames1;
                        Table(K).ColNames                         = ColNames1;


                        
                        Table(K).Catalog = [Table(K).Catalog, array2table(repmat(AddColData, Nobs, 1), 'VariableNames',ColNames2)];
                       
                        % Insert ID:
                        MidJD = (Tmp(Itmp).JD(1) + Tmp(Itmp).JD(end)).*0.5;
                        TmpP = split(ProjName,'.');
                        Node  = str2double(TmpP{2});
                        Mount = str2double(TmpP{3});
                        Cam   = str2double(TmpP{4});
                        ID  = db.Db.generateID({'sci','merged',Node, Mount, Cam, CropID, MidJD});
                        ObsNumber = (1:1:Nobs).';
                        ID  = repmat(ID, Nobs, 1);
                        Table(K).Catalog = [Table(K).Catalog, table(ID, ObsNumber)];

                        Table(K).ColNames = ColNames;
                        Table(K).Name = Obj(Iobj).FileName;
                    end
                % case 'table'
                %     % prep table of observations - all in one table
                %     Ntmp = numel(Tmp);
                %     for Itmp=1:1:Ntmp
                %         Args.AstIndex = Args.AstIndex + 1;
                %         K = K + 1;
                %         Nobs = Tmp(Itmp).Npt;
                %         % [JD, RA, Dec, FitRA, FitDec, RMS, Mag, Flags, AstIndex, DistMP]
                %         TmpTab = table(Tmp(Itmp).JD,...
                %                        Tmp(Itmp).RA,...
                %                        Tmp(Itmp).Dec,...
                %                        Tmp(Itmp).FitRA,...
                %                        Tmp(Itmp).FitDec,...
                %                        Tmp(Itmp).RMS.*ones(Nobs,1),...
                %                        Obj(Iobj).Data.(Args.FieldMag)(Tmp(Itmp).Ind),...
                %                        Obj(Iobj).Data.(Args.FieldSN)(Tmp(Itmp).Ind),...
                %                        Obj(Iobj).Data.(Args.FieldFlag)(Tmp(Itmp).Ind),...
                %                        Args.AstIndex.*ones(Nobs,1),...
                %                        Tmp(Itmp).ProperMotion.*ones(Nobs,1),...
                %                        DistMP(Itmp).*ones(Nobs,1),...
                %                        repmat(Args.DataPath,Nobs,1),...
                %                   'VariableNames',ColNames);
                % 
                %         Table = [Table; TmpTab];
                %     end
                otherwise
                    error('Unknown OutType option');                                
            end
                          
        end
            
    end
    AstIndex = Args.AstIndex;

end
