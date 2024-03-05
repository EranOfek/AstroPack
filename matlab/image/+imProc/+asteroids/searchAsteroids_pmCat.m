function [CatPM, AstCrop] = searchAsteroids_pmCat(CatPM, Args)
    % Search asteroids in merged AstroCatalog objects which contains proper motion
    %   per source.
    %   Objects with high/significant proper motions are selected and then
    %   are linked.
    %   A column is added to the input catalog with the indices of the
    %   linked asteroids [NaN - not asteroid, negative number - unlinked
    %   asteroid, pos number - linked sources].
    %   Also a cutouts of images around each detected asteroid is
    %   constructed.
    % Input  : - An AstroCatalog object.
    %            Ecah element corresponds to one field.
    %            Each catalog must contains proper motion fit per source.
    %            The proper motion information was obtained from multipl
    %            images/catalogs.
    %            RA/Dec must be in degrees!
    %          * ...,key,val,...
    %            'BitDict' - A bit dictionary by which to screen the FLAGS
    %                   information in the input catalog. If empty, then
    %                   the FLAGS information will not be used.
    %                   Default is [].
    %            'Images' - An AstroImage array of images. The rows
    %                   corresponds to the epochs, while columns
    %                   corresponds to fields (i.e., the input AstroCatalog
    %                   elements). I.e., AstroCatalog(I) corresponds to
    %                   AstroImage(:,I). These are the images from which
    %                   the matched catalog (per field) with proper motion
    %                   measurments were constructed (e.g., using 
    %                   imProc.match.mergeCatalogs).
    %                   If empty, then cutouts for the asteroids are not
    %                   generated. Default is [].
    %            'ColNameRA' - Column name in the input Astrocatalog
    %                   containing the RA of sources at a fixed epochs
    %                   (given in 'JD'). Default is 'RA'.
    %            'ColNameDec' - Like 'ColNameRA', but for Declination.
    %                   Default is 'Dec'.
    %            'ColNamePM_RA' - Like 'ColNameRA', but for proper motion
    %                   in RA.
    %                   The units of the columns should be consistent.
    %            'ColNamePM_Dec' - Like 'ColNameRA', but for proper motion
    %                   in Dec.
    %            'ColNamePM_TdistProb' - Like 'ColNameRA', but a column
    %                   that provide the probability that the alternative
    %                   hypothesis (star is moving) is correct.
    %                   Default is 'PM_TdistProb'.
    %            'ColNameNobs' - Like 'ColNameRA', but for a columns with
    %                   the number of epochs in which the source was detected.
    %                   Default is 'Nobs'.
    %            'ColNameFlags' - Like 'ColNameRA', but for a columns with
    %                   the combined FLAGS (propagated from the bit maks)
    %                   over all epochs.
    %                   Default is 'FLAGS'.
    %            'ColNameMeanSN' - Like 'ColNameRA', but for a columns with
    %                   the Mean S/N. The Mean S/N is used with the 
    %                   'HighSNBitNames' and 'SN_HighSN' arguments.
    %                   Default is 'Mean_SN_3'
    %            'ColNameStdSN' - Like 'ColNameRA', but for a columns with
    %                   the Std S/N. This isused with the 'MinStdSN'
    %                   argument. Default is 'Std_SN_3'.
    %
    %            'JD' - Vector of JD of the epochs.
    %                   If empty, then will be set to (1:Nepochs).
    %                   Note that without providing this parameter the
    %                   proper motions will be in arbitrary units.
    %                   Default is [].
    %
    %            'RemoveBitNames' - A cell array of bit names in the
    %                   'FLAGS' column. A source for which one of these
    %                   bits are on, will be removed from the list of
    %                   candidates asteroids.
    %                   Default is
    %                   {'Saturated', 'Spike', 'CR_DeltaHT', 'CR_Laplacian', 'CR_Streak', 'Streak', 'Ghost', 'Persistent', 'NearEdge'};
    %            'HighSNBitNames' - A cell array of bit names in the
    %                   'FLAGS' column.. A source for which one of these
    %                   bits are on and Mean S/N (as defined by the 
    %                   'ColNameMeanSN' argument) is smaller than
    %                   the value provided in the 'SN_HighSN' argument,
    %                   will be removed from the list of candidates asteroids.
    %                   Default is
    %                   {'DarkHighVal','BiasFlaring','FlatHighStd','HighRN'}; 
    %            'SN_HighSN' - Threshold S/N for the 'HighSNBitNames' argument.
    %                   Default is 7.
    %
    %            'TimeSpan' - The range between the mid of the first and last
    %                   observations. If empty then calc from max(JD) - min(JD).
    %                   Default is [].
    %            'PM_Radius' - This is the matching radius that was used to
    %                   generate the merged catalogs.
    %                   Default is 3.
    %            'PM_RadiusUnits' - Units for 'PM_Radius'.
    %                   Default is 'arcsec'.
    %            'Nobs_TdistProb' - a 2x2 matrix with:
    %                   [Nobs TdistProb] columns for selecting moving siurces.
    %                   Default is [5 0.995; 3 0.9999].
    %            'MinStdSN' - Sources with Std S/N (as provided by the 
    %                   'ColNameStdSN' argument) below this value will be
    %                   removed from the asteroid candidates list
    %                   (possibly persistent artifacts).
    %                   Default is 0.4.
    %            'H1_NoutlierLimit' - Maximum number of H1 PM outliers
    %                   allowed. Default is 1.
    %
    %            'LinkingRadius' - Search radius for linking. Asteroid
    %                   candidates are linked if their RA/Dec in common epoch
    %                   (this is the epoch of proper motion) is within this
    %                   search radius. Default is 7.
    %            'LinkingRadiusUnits' - Units for 'LinkingRadius'.
    %                   Default is 'arcsec'.
    %            'AddLinkingCol' - A logical indicating if to add a column
    %                   to the merged AstroCatalog with the index of linked
    %                   objects. Indices for linked objects are allocated
    %                   as following:
    %                   NaN - not an asteroid candidate.
    %                   1,2,3,... - Linked object with more than one
    %                       PM detection (such objects has multiple entries
    %                       in the input merged catalog.
    %                   -1,-2,... - Asteroid candidates that has a single
    %                       entry in the input merged catalog.
    %                   Default is true.
    %            'LinkingColName' - A column name in which to insert the
    %                   linking information.
    %                   Default is 'LinkedAsteroid'
    %
    %            'HalfSizeXY' - The half size in [X, Y] of the cropped stamp
    %                   to be cut around each asteroid candidate.
    %            'cropLonLatArgs' - A cell array of additional arguments to
    %                   pass to AstroImage/cropLonLat. Default is {}.
    %            'UseMovingSource' - A logical indicating if to produce a
    %                   MovingSource object or AstCrop structure.
    %                   Default is true.
    % Output : - The original input merged catalog, with possibly additional
    %            column 'LinkingColName' for asteroid candidates.
    %          - A structure array with the cropped images. The following
    %            fields are available:
    %            .FieldIndex - Index of field in which asteroid was
    %                   detected. A field index is the index of input
    %                   AstroCatalog element.
    %            .AstIndex - Unique asteroid index. See 'AddLinkingCol'
    %                   argument for more info.
    %            .RA - RA of requested image stamp.
    %            .Dec - Dec of requested image stamp.
    %            .Stamps - An AstroImage containing the stamps at the
    %                   asteroid position (image per epoch).
    %            .X - Requested X position of stamp center.
    %            .Y - Requested Y position of stamp center.
    %            .CCDSEC - Actual CCDSEC from original image of each stamp
    %                   (one epoch per line).
    %            .IndexOfAstInCatPM - A vector of indices (line numbers)
    %                   of the asteroid
    %                   detection in the input merged catalog.
    %            .SelectedCatPM - An AstroCatalog object with the rows
    %                   corresponding to the asteroid candidate.
    %            .JD - The JD of the stamps and original images epochs.
    % Author : Eran Ofek (Nov 2021)
    % Example: [MergedCat, AstCrop] = imProc.asteroids.searchAsteroids_pmCat(MergedCat, 'BitDict',AllSI(1).MaskData.Dict, 'JD',JD, 'PM_Radius',3, 'Images',AllSI)
    
    arguments
        CatPM AstroCatalog
        
        Args.BitDict(1,1) BitDictionary   = BitDictionary;
        Args.Images                       = [];   % column per CatPM element
        
        Args.ColNameRA                    = 'RA';  % RA at central epoch
        Args.ColNameDec                   = 'Dec'; % Dec at central epoch
        Args.ColNamePM_RA                 = 'PM_RA';
        Args.ColNamePM_Dec                = 'PM_Dec';
        Args.ColNamePM_TdistProb          = 'PM_TdistProb';
        Args.ColNameNobs                  = 'Nobs';
        Args.ColNameNoutlier              = 'Noutlier';
        Args.ColNameFlags                 = 'FLAGS';
        Args.ColNameMeanSN                = 'Mean_SN_3';  
        Args.ColNameStdSN                 = 'Std_SN_3';   
        
        Args.JD                           = [];
        
        Args.RemoveBitNames               = {'Saturated', 'Spike', 'CR_DeltaHT', 'CR_Laplacian', 'CR_Streak', 'Streak', 'Ghost', 'Persistent', 'NearEdge'};
        Args.HighSNBitNames               = {'DarkHighVal','BiasFlaring','FlatHighStd','HighRN'};   
        Args.SN_HighSN                    = 8;                                                      
        Args.TimeSpan                     = [];  % same units as PM time
        Args.PM_Radius                    = 3;   % same units as the PM
        Args.PM_RadiusUnits               = 'arcsec';
        Args.Nobs_TdistProb               = [10 0.98; 5 0.995; 3 0.9999]; %     ((PM_TdistProb > 0.995 & Nobs>5) | (PM_TdistProb>0.9999 & Nobs>3));   
        Args.MinStdSN                     = 0.4;   
        Args.H1_NoutlierLimit             = 2;  

        Args.ColNameChi2Dof               = 'Mean_PSF_CHI2DOF';
        Args.MaxChi2Dof                   = 3;
        Args.SN_ForMaxChi2Dof             = 100;
        
        % linking
        Args.LinkingRadius                = 7;
        Args.LinkingRadiusUnits           = 'arcsec';
        Args.AddLinkingCol logical        = true;
        Args.LinkingColName               = 'LinkedAsteroid';
        
        % cutouts
        Args.HalfSizeXY                   = [50 50];
        Args.cropLonLatArgs               = {'DataProp',{'ImageData'}, 'DeleteProp',{'BackData','VarData'}, 'UpdateCat',true, 'UpdateWCS',true, 'cropXYargs', {}, 'UpdateHeader',true};
        
        Args.UseMovingSource logical      = true;
        Args.ColNameMergedCat             = 'MergedCatMask';
        Args.RemoveByMergedCatFlags       = {'GAIA_DRE3','PGC','GLADE'};
        Args.BitDicMergedCat              = BitDictionary('BitMask.MergedCat.Default');

        Args.LinkAst logical              = false;
    end
    
    Args.PM_Radius   = convert.angular(Args.PM_RadiusUnits, 'deg', Args.PM_Radius); % deg
    LinkingRadiusRad = convert.angular(Args.LinkingRadiusUnits, 'rad', Args.LinkingRadius);  % rad
    
    AstrCrop = [];
    
    %[MergedCat, MatchedS, Result] = pipeline.generic.mergeCatalogs(AllSI)
    
    Ncat = numel(CatPM);
    if isempty(Args.JD)
        Args.JD = (1:1:Ncat).';
    end
    Nepochs = numel(Args.JD);  % number of epochs
    
    if isempty(Args.TimeSpan)
        % calc the TimeSpan from the vector of JD
        % TimeSpoan is the range between the mid of the first and last
        % observations
        Args.TimeSpan = max(Args.JD) - min(Args.JD);
    end
    
    if Args.UseMovingSource
        AstCrop = MovingSource(); 
    end
    Icrop = 0;
    for Icat=1:1:Ncat
        % select columns from CatPM
        [Nsrc, Ncol] = sizeCatalog(CatPM(Icat));
    
        % verify that there are sources in the catalog
        if Nsrc>1

            PM           = CatPM(Icat).getCol({Args.ColNamePM_RA, Args.ColNamePM_Dec});
            PM_TdistProb = CatPM(Icat).getCol(Args.ColNamePM_TdistProb);
            Nobs         = CatPM(Icat).getCol(Args.ColNameNobs);
            Noutlier     = CatPM(Icat).getCol(Args.ColNameNoutlier);
            DecFlags     = CatPM(Icat).getCol(Args.ColNameFlags);
            InfoSN       = CatPM(Icat).getCol({Args.ColNameMeanSN, Args.ColNameStdSN});  % [Mean, Std]
            PSF_Chi2Dif  = CatPM(Icat).getCol({Args.ColNameChi2Dof});  % [Mean]

            TotPM        = sqrt(sum(PM.^2, 2));  % total PM [deg/day]
            %ExpectedNobs = Nepochs .* TotPM.*Args.TimeSpan./(2.*Args.PM_Radius);
            %ExpectedNobs = 2.*Args.PM_Radius.*Nimages./TotPM.*Args.TimeSpan;

            % remove sources with some selected flags
            if isemptyBitDict(Args.BitDict)
                Flags(Icat).Flags        = true(Nsrc,1);
                Flags(Icat).Flags_HighSN = true(Nsrc,1);
            else
                % true if good candidate
                Flags(Icat).Flags         = ~findBit(Args.BitDict, DecFlags, Args.RemoveBitNames, 'Method','any');
                Flags(Icat).Flags_HighSN  = ~findBit(Args.BitDict, DecFlags, Args.HighSNBitNames, 'Method','any');
                Flags(Icat).Flags_HighSN  = Flags(Icat).Flags_HighSN | InfoSN(:,1)>Args.SN_HighSN;
            end

            Flags(Icat).Flag_Chi2Dof = InfoSN(:,1)>Args.SN_ForMaxChi2Dof | (InfoSN(:,1)<Args.SN_ForMaxChi2Dof & PSF_Chi2Dif<Args.MaxChi2Dof);

            % Flag sources with large number of outliers in H1 (PM hypothesis)
            % This select good stars with small number of outliers
            Flags(Icat).Flag_Outlier      = Noutlier <= Args.H1_NoutlierLimit;

            % FLAGS for good asteroid candidates
            Flags(Icat).Tdist  = ((PM_TdistProb >  Args.Nobs_TdistProb(1,2) & Nobs>Args.Nobs_TdistProb(1,1)) | (PM_TdistProb>Args.Nobs_TdistProb(2,2) & Nobs>Args.Nobs_TdistProb(2,1)));
            %    Args.Nobs_TdistProb               = [5 0.995; 3 0.9999]; %     ((PM_TdistProb > 0.995 & Nobs>5) | (PM_TdistProb>0.9999 & Nobs>3));   % NEW

            Flags(Icat).LowStdSN = InfoSN(:,2)>Args.MinStdSN;
            % Args.MinStdSN                     = 0.4;   % NEW

            Flags(Icat).Nobs   = true(size(Nobs)); %Nobs<(ExpectedNobs);
            Flags(Icat).All    = Flags(Icat).Flags & ...
                                 Flags(Icat).Flags_HighSN & ...
                                 Flags(Icat).Tdist & ...
                                 Flags(Icat).Nobs & ...
                                 Flags(Icat).LowStdSN & ...
                                 Flags(Icat).Flag_Chi2Dof & ...
                                 Flags(Icat).Flag_Outlier;

            % Number of asteroid candidates
            
            AstInd   = find(Flags(Icat).All);
            NastCand = numel(AstInd);

            % Linking objects
            % Since PM epoch is the same for all objects
            % this involves only comparing the position of sources
            [RA, Dec] = CatPM(Icat).getLonLat('rad', 'ColLon',Args.ColNameRA, 'ColLat',Args.ColNameDec); % [rad]
            % select only the asteroid candidates
            CandRA  = RA(Flags(Icat).All);
            CandDec = Dec(Flags(Icat).All);

            LinkedAstIndex       = 0;
            LinkedColumn         = nan(Nsrc, 1);  % nan - no PM | negative/unique(not necessely continous) - asteroid w/o links | >0 - asteroid with links
            LinkedColumn(AstInd) = -(1:1:numel(AstInd));
            if Args.LinkAst
                % FFU: there is a bug in this section -
                % it doesn't find linked asteroids.
                if NastCand>0
                    for Icand=1:1:NastCand
                        Dist = celestial.coo.sphere_dist_fast(CandRA(Icand), CandDec(Icand), CandRA, CandDec);
                        Dist(Icand) = NaN;
    
                        FlagLink = Dist < LinkingRadiusRad;
                        if sum(FlagLink)>1
                            % found a match for asteroid 
                            LinkedAstIndex = LinkedAstIndex + 1;
    
                            % mark the linked sources as the same asteroid (same
                            % index)
                            LinkedColumn(AstInd(Icand))    = LinkedAstIndex;
                            LinkedColumn(AstInd(FlagLink)) = LinkedAstIndex;
                        end
                    end
                end
            end
            if Args.AddLinkingCol
                CatPM(Icat).insertCol(LinkedColumn, Inf, Args.LinkingColName, '');
            end
            

            % extract cutouts centered on asteroids candidates
            if ~isempty(Args.Images) 
                % for eack linked source
                find(LinkedColumn == 0);
                UniquAst = unique(LinkedColumn);
                UniquAst = UniquAst(~isnan(UniquAst));
                Nunique  = numel(UniquAst);

                % create MovingSource object
                if Args.UseMovingSource

                    for Iun=1:1:Nunique
                        % for each unique asteroid
                        % extract a cutout from Args.Images
                        Iast = find(LinkedColumn==UniquAst(Iun));

                        if isempty(Args.RemoveByMergedCatFlags)
                            StoreAsteroid = true;
                        else
                            BitDec = CatPM(Icat).getCol(Args.ColNameMergedCat);

                            if Args.BitDicMergedCat.findBit(BitDec(Iast), Args.RemoveByMergedCatFlags, 'Method','any')
                                % skip
                                StoreAsteroid = false;
                            else             
                                StoreAsteroid = true;
                            end
                        end
                        if StoreAsteroid
                            Icrop = Icrop + 1;
                            %Icrop
                            AstCrop(Icrop).MergedCat             = CatPM(Icat).selectRows(Iast);
                            %AstCrop(Icrop).JD                    = Args.JD;
                            %AstCrop(Icrop).RA             = RA(Iast(1));   % [rad]
                            %AstCrop(Icrop).Dec            = Dec(Iast(1));  % [rad]
                            [AstCrop(Icrop).Stamps, Info] = cropLonLat(Args.Images(:, Icat), AstCrop(Icrop).RA, AstCrop(Icrop).Dec,...
                                                                            'CooUnits','deg',...
                                                                            'HalfSizeXY',Args.HalfSizeXY,...
                                                                            Args.cropLonLatArgs{:});
                            
                            AstCrop(Icrop).Info.X                = Info.X;
                            AstCrop(Icrop).Info.Y                = Info.Y;
                            AstCrop(Icrop).Info.CCDSEC           = Info.CCDSEC;
                            AstCrop(Icrop).Info.FieldIndex     = Icat;
                            AstCrop(Icrop).Info.AstIndex       = UniquAst(Iun);
                            % asteroid selected lines from CatPM
                            AstCrop(Icrop).Info.IndexInMergedCat = Iast;
                                                            
                        end
                    end

                else
                    % Use original AstCrop format

                    for Iun=1:1:Nunique
                        % for each unique asteroid
                        % extract a cutout from Args.Images
    
                        Iast = find(LinkedColumn==UniquAst(Iun));
    
                        Icrop = Icrop + 1;
    
                        AstCrop(Icrop).FieldIndex     = Icat;
                        AstCrop(Icrop).AstIndex       = UniquAst(Iun);
    
                        AstCrop(Icrop).RA             = RA(Iast(1));   % [rad]
                        AstCrop(Icrop).Dec            = Dec(Iast(1));  % [rad]
                        [AstCrop(Icrop).Stamps, Info] = cropLonLat(Args.Images(:, Icat), AstCrop(Icrop).RA, AstCrop(Icrop).Dec,...
                                                                        'CooUnits','rad',...
                                                                        'HalfSizeXY',Args.HalfSizeXY,...
                                                                        Args.cropLonLatArgs{:});
                        AstCrop(Icrop).X              = Info.X;
                        AstCrop(Icrop).Y              = Info.Y;
                        AstCrop(Icrop).CCDSEC         = Info.CCDSEC;
                        % asteroid selected lines from CatPM
                        AstCrop(Icrop).IndexOfAstInCatPM = Iast;
                        AstCrop(Icrop).SelectedCatPM  = CatPM(Icat).selectRows(Iast);
                        AstCrop(Icrop).JD             = Args.JD;
    
                    end
                end
            end
        end
    end
    
    if Icrop==0
        AstCrop = [];
    end
        
end

