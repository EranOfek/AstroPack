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
    end 

    ColNames = {'JD', 'RA', 'Dec', 'FitRA', 'FitDec', 'RMS', 'Mag', 'SN', 'Flags', 'AstIndex'};
    Table    = table([],[],[],[],[],[],[],[],[],[]);
    Table.Properties.VariableNames = ColNames;

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
        if ~isempty(Tmp)
            TmpAC = AstroCatalog()
            [OnlyMP, AstCat, AC1] = imProc.match.match2solarSystem(AC1, 'JD',JD, 'GeoPos',[], 'OrbEl',OrbEl, 'SearchRadius',1, 'INPOP',IN);
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
                    Ntmp = numel(Tmp);
                    for Itmp=1:1:Ntmp
                        Args.AstIndex = Args.AstIndex + 1;
                        K = K + 1;
                        if K==1
                            Table = AstroCatalog;
                        end
                        Nobs = Tmp(Itmp).Npt;
                        Table(K).Catalog = [Tmp(Itmp).JD,...
                               Tmp(Itmp).RA,...
                               Tmp(Itmp).Dec,...
                               Tmp(Itmp).FitRA,...
                               Tmp(Itmp).FitDec,...
                               Tmp(Itmp).RMS.*ones(Nobs,1),...
                               Obj(Iobj).Data.(Args.FieldMag)(Tmp(Itmp).Ind),...
                               Obj(Iobj).Data.(Args.FieldSN)(Tmp(Itmp).Ind),...
                               Obj(Iobj).Data.(Args.FieldFlag)(Tmp(Itmp).Ind),...
                               Args.AstIndex.*ones(Nobs,1)];
                        Table(K).ColNames = ColNames;
                        Table(K).Name = Obj(Iobj).FileName;
                    end
                case 'table'
                    % prep table of observations - all in one table
                    Ntmp = numel(Tmp);
                    for Itmp=1:1:Ntmp
                        Args.AstIndex = Args.AstIndex + 1;
                        K = K + 1;
                        Nobs = Tmp(Itmp).Npt;
                        % [JD, RA, Dec, FitRA, FitDec, RMS, Mag, Flags, AstIndex]
                        TmpTab = table(Tmp(Itmp).JD,...
                                       Tmp(Itmp).RA,...
                                       Tmp(Itmp).Dec,...
                                       Tmp(Itmp).FitRA,...
                                       Tmp(Itmp).FitDec,...
                                       Tmp(Itmp).RMS.*ones(Nobs,1),...
                                       Obj(Iobj).Data.(Args.FieldMag)(Tmp(Itmp).Ind),...
                                       Obj(Iobj).Data.(Args.FieldSN)(Tmp(Itmp).Ind),...
                                       Obj(Iobj).Data.(Args.FieldFlag)(Tmp(Itmp).Ind),...
                                       Args.AstIndex.*ones(Nobs,1),...
                                  'VariableNames',ColNames);
                        
                        Table = [Table; TmpTab];
                    end
                otherwise
                    error('Unknown OutType option');                                
            end
                          
        end
            
    end
    AstIndex = Args.AstIndex;

end
