% MovingSource class
%   MinorPlanet class can be used to store detection of minor planet
%   (or any moving) objects.
% Description: 
% Author : Eran Ofek (Jan 2024)
%
%
%

classdef MovingSource < Component
    
  
 
    properties (Dependent) % Access image data directly
    end
    
    properties (SetAccess = public)
        JD
        RA
        Dec
        PM_RA
        PM_Dec
        CooUnits
        PMUnits
        Mag
        ImageID     = []; % struct with fields defined by KeyID
        Stamps      = AstroImage;
        MergedCat
        KnownAst    = [];
        FileName    = '';
    end

    properties (Hidden, SetAccess = public)
        KeyID     = {'NODENUMB','MOUNTNUM','CAMNUM','CROPID','FIELDID','COUNTER'};
        KeyJD     = 'JD';
        ColRA     = 'RA';
        ColDec    = 'Dec';
        ColPM_RA  = 'PM_RA';
        ColPM_Dec = 'PM_Dec';
        ColMag    = 'Mean_MAG_PSF';

        %PopulatedKnownAst logical = false;
    end

    properties (Hidden, SetAccess = public)
        OrbEl     = celestial.OrbitalEl.loadSolarSystem('merge');
        INPOP     = celestial.INPOP.init;
        AstSearchRadius       = 10;
        AstSearchRadiusUnits  = 'arcsec';
        AstMagLimit           = Inf;
    end
    
    
    methods % Constructor
       
        function Obj = MinorPlanet(varargin)
            % Constructor of MinorPlanet class using the superclass
            % (ImageComponent) constructor
            
            %Obj@ImageComponent(varargin{:});
            %for Iobj=1:1:numel(Obj)
            %    Obj(Iobj).DataType = AstroDataType.ExpTime;
            %end
            
        end



    end
 
    methods % getters/setters
        function Val=get.ImageID(Obj)
            % getter for ImageID
            % Read info from stamps header

            if isempty(Obj.ImageID)
                IsEmptyImage = isemptyImage(Obj.Stamps);
                if all(IsEmptyImage)
                    % do nothing
                    Val = [];
                else
                    % read ImageID from header
                    Nstamps = numel(Obj.Stamps);
                    Val = getStructKey(Obj.Stamps, Obj.KeyID);
                end
            else
                % do nothing
                Val = Obj.ImageID;
            end
            Obj.ImageID = Val;
        end


        function Val=get.JD(Obj)
            % getter for JD

            Key = 'JD';
            KeyName = 'JD';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = Obj.MergedCat.(KeyName);
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.RA(Obj)
            % getter for RA

            Key = 'RA';
            ColName = 'ColRA';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getCol(Obj.MergedCat, Obj.(ColName));
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.Dec(Obj)
            % getter for Dec

            Key = 'Dec';
            ColName = 'ColDec';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getCol(Obj.MergedCat, Obj.(ColName));
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.CooUnits(Obj)
            % getter for CooUnits

            Key = 'CooUnits';
            ColName = 'ColRA';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    [~,Tmp] = getCol(Obj.MergedCat, Obj.(ColName));
                    Val = Tmp{1};
                end
            else
                Val = Obj.(Key);
            end
        end

        function Val=get.PM_RA(Obj)
            % getter for PM_RA

            Key = 'PM_RA';
            ColName = 'ColPM_RA';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getCol(Obj.MergedCat, Obj.(ColName));
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.PM_Dec(Obj)
            % getter for PM_Dec

            Key = 'PM_Dec';
            ColName = 'ColPM_Dec';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getCol(Obj.MergedCat, Obj.(ColName));
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.PMUnits(Obj)
            % getter for PMUnits

            Key = 'PMUnits';
            ColName = 'ColPM_RA';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    [~,Tmp] = getCol(Obj.MergedCat, Obj.(ColName));
                    Val = Tmp{1};
                end
            else
                Val = Obj.(Key);
            end
        end

        function Val=get.Mag(Obj)
            % getter for Mag

            Key = 'Mag';
            ColName = 'ColMag';

            if isempty(Obj.(Key))
                if isempty(Obj.MergedCat)
                    Val = [];
                else
                    Val = getCol(Obj.MergedCat, Obj.(ColName));
                end
            else
                Val = Obj.(Key);
            end
            
        end

        function Val=get.KnownAst(Obj)
            % getter for KnownAst
            if ~isempty(Obj.JD) && ~isempty(Obj.RA) && ~isempty(Obj.Dec)
                [Result] = searchMinorPlanetsNearPosition(Obj.OrbEl, Obj.JD, Obj.RA, Obj.Dec, Obj.AstSearchRadius,...
                                    'INPOP',Obj.INPOP,...
                                    'CooUnits',Obj.CooUnits,...
                                    'SearchRadiusUnits',Obj.AstSearchRadiusUnits,...
                                    'MagLimit',Obj.AstMagLimit);
                Val = Result;
                %Obj.PopulatedKnownAst = true;
            else
                Val = Obj.KnownAst;
            end

        end

    end

    methods (Static)  % static methods
        % conversions
        function Obj=readFromAstCrop(AstCrop, Args)
            % Read AstCrop structure (old format) into MovingSource object
            % Input  : - AstCrop strtucture, or mat file name containing
            %            AstCrop structure.
            %            If empty, then recursivly search for all files
            %            indicated by template 'AstFileTemp'.
            %            Default is [].
            %          * ...,key,val,...
            %            'Path' - Move to this path. Default is pwd.
            %            'Id' - Id of AstCrop element. If empty, read all.
            %                   Default is [].
            %            'AstFileTemp' - File template to search.
            %                   Default is '*merged_Asteroids*.mat'.
            % Output : - A MovingSource object.
            % Example: MP=MovingSource.readFromAstCrop(AC)
            %          MP=MovingSource.readFromAstCrop('LAST.01.10.01_20231107.010845.394_clear_091+10_001_001_001_sci_merged_Asteroids_1.mat')
            %          MP=MovingSource.readFromAstCrop()

            arguments
                AstCrop          = [];
                Args.Path        = pwd;
                Args.Id          = [];
                Args.AstFileTemp = '*merged_Asteroids*.mat';
            end

            PWD = pwd;
            cd(Args.Path);

            if isempty(AstCrop)
                Files = io.files.rdir(Args.AstFileTemp);
            elseif ischar(AstCrop) || isstring(AstCrop)
                Files(1).folder = pwd;
                Files(1).name   = AstCrop;
            else
                % do nothing
            end

            Obj = MovingSource;
            
            

            if isstruct(AstCrop)
                Ncrop = numel(AstCrop.AstCrop);
                Iall = 0;
                for Icrop=1:1:Ncrop
                    if isempty(Args.Id) || Icrop==Args.Id
                        Iall = Iall + 1;
                        Obj(Iall).Stamps    = AstCrop.AstCrop(Icrop).Stamps;
                        Obj(Iall).MergedCat = AstCrop.AstCrop(Icrop).SelectedCatPM;
                    end
                end

            else
                Nf = numel(Files);
                Iall = 0;
                for If=1:1:Nf
                    AstCrop = io.files.load2(fullfile(Files(If).folder, Files(If).name));
                    Ncrop   = numel(AstCrop.AstCrop);
                    for Icrop=1:1:Ncrop
                        if isempty(Args.Id) || Icrop==Args.Id
                            Iall = Iall + 1;
                            Obj(Iall).Stamps    = AstCrop.AstCrop(Icrop).Stamps;
                            Obj(Iall).MergedCat = AstCrop.AstCrop(Icrop).SelectedCatPM;
                        end
                    end
                end
            end

            cd(PWD);
        end

    end

    methods  % utilities
        function Flag=nearStatisSrc(Obj, Args)
            % Check for static sources near the moving source
            %   Search a PGC galaxy or GAIA star near the moving source.
            %   The PGC search is based on the galaxy radius.
            %   The GAIA search distance is magnitude dependent (e.g.,
            %   brighter GAIA stars have larger radius of influence).
            % Input  : - A MovingSource object.
            %          * ...,key,val,...
            %            See code for details.
            % Output : - A structure array (element per MovingSource
            %            element) with the following fields:
            %            .GAIA - A logical indicating if there is a GAIA
            %                   star that may contaminate the detection.
            %            .MagGAIA - The mag. of the GAIA contaminants.
            %            .PGC - A logical indicating if there is a PGC
            %                   galaxy that may contaminate the detection.
            %            .Flag - A logical flag indicating if there is a
            %                   contaminant of any kind.
            % Author : Eran Ofek (Jan 2024)
            % Example: Res=MP(3).nearStatisSrc
            arguments
                Obj
                Args.CatNameGAIA = 'GAIADR3';
                Args.CatNamePGC  = 'PGC';
                Args.ColMagGAIA  = 'phot_bp_mean_mag';
                Args.MaxMagGAIA  = 20.5;

                Args.SearchRadius      = 60;
                Args.SearchRadiusUnits = 'arcsec';

                

            end

            Nobj = numel(Obj);
            for Iobj=1:1:Nobj
                %
                
                CooFactor = convert.angular(Obj(Iobj).CooUnits, 'rad');
                RA_rad  = Obj(Iobj).RA.*CooFactor;   % [rad]
                Dec_rad = Obj(Iobj).Dec.*CooFactor;  % [rad]

                % GAIA
                Flag(Iobj).GAIA = false;
                Flag(Iobj).MagGAIA = [];
                CatGAIA = catsHTM.cone_search(Args.CatNameGAIA, RA_rad, Dec_rad, Args.SearchRadius, 'RadiusUnits',Args.SearchRadiusUnits, 'OutType','AstroCatalog');
                Dist    = CatGAIA.sphere_dist(RA_rad, Dec_rad, 'rad', 'arcsec');
                DistThresholdPerStar = max(20-CatGAIA.Table.(Args.ColMagGAIA), 3);
                FlagM   = CatGAIA.Table.(Args.ColMagGAIA)<Args.MaxMagGAIA & Dist<DistThresholdPerStar;

                if any(FlagM)
                    Flag(Iobj).GAIA = true;
                    Flag(Iobj).MagGAIA = CatGAIA.Table.(Args.ColMagGAIA)(FlagM);
                end

                % PGC
                Flag(Iobj).Galaxy = false;
                CatPGC = catsHTM.cone_search(Args.CatNamePGC, RA_rad, Dec_rad, Args.SearchRadius, 'RadiusUnits',Args.SearchRadiusUnits, 'OutType','AstroCatalog');
                Dist    = CatPGC.sphere_dist(RA_rad, Dec_rad, 'rad', 'arcsec');
                
                GalRadius = 3.*10.^(CatPGC.Table.LogD25);
                if ~isempty(Dist) && any(Dist<GalRadius)
                    Flag(Iobj).Galaxy = true;
                end
                
                Flag(Iobj).Flag = Flag(Iobj).GAIA || Flag(Iobj).Galaxy;
            end

            

        end

    end

    methods % display and plot
        function Info=dispInfo(Obj, Args)
            % Display moving source information on screen
            % Input  : - A MovingSource object.
            %          * ...,key,val,...
            %            See code for options.
            % Output : - Info structure (not yet available).
            % Author : Eran Ofek (Jan 2024)
            % Example: MP(1).dispInfo;

            arguments
                Obj

                Args.Plot logical  = true;
                Args.BitDic        = BitDictionary;
                Args.CatBitDic     = BitDictionary('BitMask.MergedCat.Default');
            end

            Info = [];
            Nobj = numel(Obj);
            for Iobj=1:1:Nobj

                if Args.Plot
                    fprintf('File %s loaded\n',Obj(Iobj).FileName);
                    fprintf('ID for 1st stamp:\n')
                    FN = fieldnames(Obj(Iobj).ImageID(1));
                    Nfn = numel(FN);
                    for Ifn=1:1:Nfn
                        Val = Obj(Iobj).ImageID(1).(FN{Ifn});
                        if isnumeric(Val)
                            if Val==floor(Val)
                                % integer
                                fprintf('  %-11s : %d\n', FN{Ifn}, Val);
                            else
                                % float:
                                fprintf('  %-11s : %f\n', FN{Ifn}, Val);
                            end
                        else
                            % char
                            fprintf('  %-11s : %s\n', FN{Ifn}, Val);
                        end
                    end
                    fprintf('RA            : %s\n', celestial.coo.convertdms(Obj(Iobj).RA, 'd', 'SH'));
                    fprintf('Dec           : %s\n',celestial.coo.convertdms(Obj(Iobj).Dec, 'd', 'SD'));
                    
                    fprintf('PM_RA         : %f [time-deg/day]\n',Obj(Iobj).PM_RA);
                    fprintf('PM_Dec        : %f [deg/day]\n',Obj(Iobj).PM_Dec);
                    fprintf('JD_PM         : %15.6f\n',Obj(Iobj).JD);
                    
                    % LAST specific
                    fprintf('PM_TdistProb  : %8.6f\n',Obj(Iobj).MergedCat.Table.PM_TdistProb);
                    fprintf('Nobs          : %d\n',Obj(Iobj).MergedCat.Table.Nobs);
                    fprintf('Noutlier      : %d\n',Obj(Iobj).MergedCat.Table.Noutlier);

                    fprintf('MAG mean      : %f\n', Obj(Iobj).Mag);

                    % LAST specific
                    fprintf('MAG range     : %f\n', Obj(Iobj).MergedCat.Table.Range_MAG_PSF);
                    fprintf('S/N mean      : %f\n', Obj(Iobj).MergedCat.Table.Mean_SN_3);
                
                    
                    FlagsName = Args.BitDic.bitdec2name(Obj(Iobj).MergedCat.Table.FLAGS);
                    FlagsName = FlagsName{1};
                    fprintf('FLAGS         :');
                    for Ifn=1:1:numel(FlagsName)
                        fprintf('  %s', FlagsName{Ifn});
                    end
                    fprintf('\n');
                    FlagsName = Args.CatBitDic.bitdec2name(Obj(Iobj).MergedCat.Table.MergedCatMask);
                    FlagsName = FlagsName{1};
                    fprintf('MergedCat     :');
                    for Ifn=1:1:numel(FlagsName)
                        fprintf('  %s', FlagsName{Ifn});
                    end
                    fprintf('\n');
                    fprintf('PolyDeltaChi2 : %f\n', Obj(Iobj).MergedCat.Table.PolyDeltaChi2);
                    
                end
            end

        end
    

        function blink1(Obj, Args)
            % Display stamps along with sources, asteroid, and known minor planets positions and blink
            %   the images.
            %   
            % Input  : - A single element MovingSource object.
            %

            arguments
                Obj(1,1)
                Args.Ndisp             = 2; % number of stamps to display
                Args.Zoom              = 'to fit';
                Args.PlotSrc           = 'sg';
                Args.PlotAst           = 'sr';
                Args.PlotKnown         = 'or';
                Args.PlotIndiv         = {'sc','Size',8};
                Args.MaxDistIndiv      = 3; % arcsec
                Args.AutoBlink         = true;

                Args.DispInfo logical  = true;
            end
            ARCSEC_DEG = 3600;

            if ~iscell(Args.PlotSrc)
                Args.PlotSrc = {Args.PlotSrc};
            end
            if ~iscell(Args.PlotAst)
                Args.PlotAst = {Args.PlotAst};
            end
            if ~iscell(Args.PlotKnown)
                Args.PlotKnown = {Args.PlotKnown};
            end
            if ~iscell(Args.PlotIndiv)
                Args.PlotIndiv = {Args.PlotIndiv};
            end
            
            Nstamp = numel(Obj.Stamps);
            Nstep  = Nstamp./(Args.Ndisp-1) - 1;
            StampVec = (1:Nstep:Nstamp);
            NstV     = numel(StampVec);
            for IstV=1:1:NstV
                IndS = StampVec(IstV);

                if Args.DispInfo
                    % display information:
                    Obj.dispInfo;
                end

                %ds9.single;
                ds9(Obj.Stamps(IndS), IstV);
                ds9.zoom(Args.Zoom)
                if ~isempty(Args.PlotSrc)
                    ds9.plot(Obj.Stamps(IndS),Args.PlotSrc{:});
                end
                if ~isempty(Args.PlotAst)
                    ds9.plotc(Obj.MergedCat, Args.PlotAst{:});
                end
                if ~isempty(Args.PlotSrc)
                    if ~isempty(Obj.KnownAst)
                        if Obj.KnownAst.sizeCatalog>0
                            ds9.plotc(Obj.KnownAst, Args.PlotKnown{:});
                        end
                    end
                end
                if ~isempty(Args.PlotIndiv)
                    VecRA  = nan(Nstamp,1);
                    VecDec = nan(Nstamp,1);

                    for Istamp=1:1:Nstamp
                        DistDeg = Obj.Stamps(Istamp).CatData.sphere_dist(Obj.RA, Obj.Dec, Obj.CooUnits, 'deg');
                        [MinDist,Imin] = min(DistDeg);
                        if ~isempty(Imin) && (MinDist.*ARCSEC_DEG)<Args.MaxDistIndiv
                            VecRA(Istamp) = Obj.Stamps(Istamp).CatData.Table.RA(Imin);
                            VecDec(Istamp) = Obj.Stamps(Istamp).CatData.Table.Dec(Imin);
                        end
                    end
                    ds9.plotc(VecRA, VecDec, Args.PlotIndiv{:});
                     
                end

            end
            ds9.match_wcs;
            if Args.AutoBlink
                ds9.blink;
            end
            
        end

    end

    
    methods (Static) % Unit-Test
        Result = unitTest()
            % unitTest for BackImage class

    end
    
end

           
